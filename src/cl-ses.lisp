(in-package :cl-user)
(defpackage cl-ses
  (:use :cl)
  (:export :send-email))
(in-package :cl-ses)

;;-----------------------------------------------------------------------------
;; SEND EMAIL
;;-----------------------------------------------------------------------------

(defun send-email (&key from to subject message aws-access-key aws-secret-key)
  (let* ((raw-message (make-raw-message from to subject message))
         (raw-message-encoded (base64-encode (string-to-utf8vector raw-message)
                                             :line-separated t))
         (request (concatenate 'string "Action=SendRawEmail&RawMessage.Data="
                               (escape-base64-string raw-message-encoded))))
    (send-request request aws-access-key aws-secret-key)))


(defparameter +email-template+
    "From: ~A~%~
   To: ~A~%~
   Subject: ~A~%~
   Content-Type: text/plain; charset=utf-8~%~
   Content-Transfer-Encoding: quoted-printable~%~
   ~%~
   ~A~%")

(defun make-raw-message (from to subject message)
  (let ((subj (subject-as-base64 subject))
        (msg (escape-as-quoted-printable message)))
    (format nil +email-template+ from to subj msg)))

(defun subject-as-base64 (subject)
  (concatenate 'string
               "=?utf-8?B?"
               (base64-encode (string-to-utf8vector subject))
               "?="))


;;-----------------------------------------------------------------------------
;; SEND SES POST REQUEST
;;-----------------------------------------------------------------------------

(defparameter +ses-host+ "email.us-west-2.amazonaws.com")

(defvar +newline+
  (format nil "~C~C" (code-char 13) (code-char 10)))

(defun send-request (request-data aws-access-key aws-secret-key)
  (let* ((uri (concatenate 'string "https://" +ses-host+))
         (sig-headers (prepare-sig-header request-data aws-access-key aws-secret-key))
         (status (nth-value 1 (drakma:http-request uri
                                                   :method :POST
                                                   :content-type "application/x-www-form-urlencoded"
                                                   :content request-data
                                                   :additional-headers sig-headers))))
    (cond
      ((= 200 status) t)
      ((= 400 status) nil)
      (t nil))))


(defun prepare-sig-header (request-data aws-access-key aws-secret-key)
  (let* ((date-str (get-date-string))
         (date-vec (string-to-vector date-str))
         (key-vec (string-to-vector aws-secret-key))
         (signature-str (base64-encode (hmac-sha1 key-vec date-vec)))
         (sig-list nil))
    (setf sig-list (acons '("X-Amzn-Authorization")
                          (format
                           nil
                           "POST / HTTP/1.1~A~
                           Host: ~A~A~
                           Content-Type: application/x-www-form-urlencoded~A~
                           Date: ~A~A~
                           Content-Length: ~D~A~
                           X-Amzn-Authorization: AWS3-HTTPS AWSAccessKeyId=~A, Algorithm=HmacSHA1, Signature=~A~A~
                           ~A~
                           ~A"
                           +newline+
                           +ses-host+ +newline+
                           +newline+
                           date-str +newline+
                           (length request-data) +newline+
                           aws-access-key signature-str +newline+
                           +newline+
                           request-data)
                          sig-list))
    sig-list))


(defun get-date-string ()
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~A, ~2,'0D ~A ~D ~2,'0D:~2,'0D:~2,'0D GMT"
            (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            date
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            year hour minute second)))


;;-----------------------------------------------------------------------------
;; PARSE SES RESPONSE STREAM
;;-----------------------------------------------------------------------------

(defun parse-response (stream)
  (let* ((status (response-read-status-line stream))
         (content-length (response-read-content-length-and-skip-headers stream))
         (content (response-read-content stream content-length)))
    (values status content)))

(defun response-read-status-line (stream)
  (let ((code (parse-integer (read-line stream) :start 9 :end 12 :junk-allowed t)))
    (cond
      ((= 200 code) t)
      ((= 400 code) nil)
      (t nil))))

(defun response-read-content-length-and-skip-headers (stream)
  (let ((len 0))
    (do ((line (read-line stream) (read-line stream)))
        ((= 1 (length line)))
      (when (and (< 14 (length line))
                 (string= (string-upcase (subseq line 0 14)) "CONTENT-LENGTH"))
        (setq len (parse-integer line :start 16 :junk-allowed t))))
    len))

(defun response-read-content (stream len)
  (let ((content (make-string len)))
    (dotimes (i len)
      (setf (char content i) (read-char stream)))
    content))


;;-----------------------------------------------------------------------------
;; LW STRING TO OCTET VECTOR CONVERTIONS
;;-----------------------------------------------------------------------------

(defun string-to-vector (str)
  (let ((buffer (make-array (length str))))
    (dotimes (i (length str))
      (let ((code (char-code (char str i))))
        (if (< code 256)
            (setf (aref buffer i) code)
            (error "String ~s contains non latin-1 characters" str))))
    buffer))

(defun string-to-utf8vector (str)
  (let ((buffer (make-array (length str) :adjustable t :fill-pointer 0)))
    (dotimes (i (length str))
      (let ((code (char-code (char str i))))
        (cond
          ((< code #x80)
           (vector-push-extend code buffer))
          ((< code #x800)
           (vector-push-extend (logior #b11000000 (ash (logand #b11111000000 code) -6)) buffer)
           (vector-push-extend (logior #b10000000 (logand #b111111 code)) buffer))
          ((< code #x10000)
           (vector-push-extend (logior #b11100000 (ash (logand #b1111000000000000 code) -12)) buffer)
           (vector-push-extend (logior #b10000000 (ash (logand #b111111000000 code) -6)) buffer)
           (vector-push-extend (logior #b10000000 (logand #b111111 code)) buffer))
          (t (error "Character is out of the ucs-2 range")))))
    buffer))


;;-----------------------------------------------------------------------------
;; QUOTED PRINTABLE
;; Note: all linebreaks in text are quoted
;;-----------------------------------------------------------------------------

(defvar +bytes-per-line+ 25) ; quoted line is no more than 76 chars
(defvar +hex-chars+ "0123456789ABCDEF")

(defun escape-as-quoted-printable (str)
  (let* ((vector (string-to-utf8vector str))
         (quoted-string (make-array (length vector)
                                    :element-type 'base-char
                                    :adjustable t
                                    :fill-pointer 0))
         (counter 0))
    (dotimes (i (length vector))
      (let* ((byte (aref vector i))
             (hi (ash byte -4))
             (lo (logand #x0F byte)))
        (incf counter)
        (vector-push-extend #\= quoted-string)
        (vector-push-extend (char +hex-chars+ hi) quoted-string)
        (vector-push-extend (char +hex-chars+ lo) quoted-string)
        (when (> counter +bytes-per-line+)
          (vector-push-extend #\= quoted-string)
          (vector-push-extend #\lf quoted-string)
          (setq counter 0))))
    quoted-string))


;;-----------------------------------------------------------------------------
;; BASE64 ENCODING
;;-----------------------------------------------------------------------------

(defvar +base64-line-length+ 76) ; in chars

(defun base64-encode (buf &key (line-separated nil))
  (let* ((buflen (length buf))
         (strlen (base64-strlen buflen line-separated))
         (str (make-array strlen :element-type 'base-char))
         (i 0)   ; position in the buffer
         (pos 0) ; position in the string
         (bytes-per-line (* 3 (/ +base64-line-length+ 4)))
         (bon 0)) ; current number of bytes in a line
    (dotimes (j (floor buflen 3))
      (let ((num (+ (ash (aref buf (+ i 0)) 16)
                    (ash (aref buf (+ i 1)) 8)
                    (aref buf (+ i 2)))))
        (setf (char str pos) (base64-char (ash num -18))
              (char str (+ pos 1)) (base64-char (ash num -12))
              (char str (+ pos 2)) (base64-char (ash num -6))
              (char str (+ pos 3)) (base64-char num))
        (incf pos 4)
        (incf i 3)
        (when line-separated         ; place lf if needed
          (incf bon 3)
          (when (and (= bon bytes-per-line) (/= pos strlen))
            (setq bon 0)
            (setf  (char str pos) #\lf)
            (incf pos)))))
    (case (rem buflen 3)
      (1 (let ((num (ash (aref buf i) 4)))
           (setf (char str pos) (base64-char (ash num -6))
                 (char str (+ pos 1)) (base64-char num)
                 (char str (+ pos 2)) #\=
                 (char str (+ pos 3)) #\=)))
      (2 (let ((num (+ (ash (aref buf i) 10)
                       (ash (aref buf (+ i 1)) 2))))
           (setf (char str pos) (base64-char (ash num -12))
                 (char str (+ pos 1)) (base64-char (ash num -6))
                 (char str (+ pos 2)) (base64-char (ash num 0))
                 (char str (+ pos 3)) #\=))))
    str))

(defvar +base64-alphabet+
  (vector #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
          #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
          #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
          #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

(defun base64-char (number)
  (svref +base64-alphabet+ (logand #b111111 number)))

(defun base64-strlen (buflen line-separated-p)
  (let ((len (+ (* 4 (floor buflen 3))
                (if (zerop (rem buflen 3)) 0 4))))
    (if line-separated-p
        (+ len (1- (ceiling len +base64-line-length+)))
        len)))


;;-----------------------------------------------------------------------------
;; ESCAPE BASE64 STRING
;;-----------------------------------------------------------------------------

(defun escape-base64-string (str)
  (let ((escaped-string (make-array (length str)
                                    :element-type 'base-char
                                    :adjustable t
                                    :fill-pointer 0)))
    (dotimes (i (length str))
      (let ((ch (char str i)))
        (case ch
          (#\cr (vector-push-extend #\% escaped-string)
                (vector-push-extend #\0 escaped-string)
                (vector-push-extend #\D escaped-string))
          (#\lf (vector-push-extend #\% escaped-string)
                (vector-push-extend #\0 escaped-string)
                (vector-push-extend #\A escaped-string))
          (#\= (vector-push-extend #\% escaped-string)
               (vector-push-extend #\3 escaped-string)
               (vector-push-extend #\D escaped-string))
          (otherwise
           (vector-push-extend ch escaped-string)))))
    escaped-string))


;;-----------------------------------------------------------------------------
;; HMAC SHA1
;;-----------------------------------------------------------------------------

(defun hmac-sha1 (key-vector msg-vector)
  (let ((key (make-key key-vector)))
    (sha1 (vec+ (opad-key key)
                (sha1 (vec+ (ipad-key key)
                            msg-vector))))))

(defun vec+ (vector1 vector2)
  (concatenate 'vector vector1 vector2))

(defvar +sha1-blocksize+ 64)

(defun make-key (key-vector &optional (block-size +sha1-blocksize+))
  (let ((key (if (> (length key-vector) block-size) (sha1 key-vector) key-vector)))
    (vec+ key (make-array (- block-size (length key)) :initial-element 0))))

(defun opad-key (key)
  (make-pad-key key #x5c))

(defun ipad-key (key)
  (make-pad-key key #x36))

(defun make-pad-key (key xor-value)
  (let ((buffer (make-array (length key))))
    (dotimes (i (length key))
      (setf (aref buffer i) (logxor xor-value (aref key i))))
    buffer))


;;-----------------------------------------------------------------------------
;; SHA1
;;-----------------------------------------------------------------------------

(declaim (inline to-32bit-word))
(defun to-32bit-word (int)
  (logand #xFFFFFFFF int))

(declaim (inline rotl))
(defun rotl (n shift)
  (logior (to-32bit-word (ash n shift))
          (ash n (- shift 32))))

(defun padding-size (n)
  (let ((x (mod (- 56 (rem n 64)) 64)))
    (if (zerop x) 64 x)))

(defun pad-message (message-vector)
  (let* ((message-len (length message-vector))
         (message-len-in-bits (* message-len 8))
         (buffer-len (+ message-len 8 (padding-size message-len)))
         (buffer (make-array buffer-len :initial-element 0)))
    (dotimes (i message-len)
      (setf (aref buffer i) (aref message-vector i)))
    (setf (aref buffer message-len) #b10000000)
    (dotimes (i 8)
      (setf (aref buffer (- buffer-len (1+ i)))
            (logand #xFF (ash message-len-in-bits (* i -8)))))
    buffer))

(defun prepare-message-block (n data)
  (let ((message-block (make-array 80))
        (offset (* n 64)))
    (do ((i 0 (1+ i)))
        ((> i 15))
      (setf (aref message-block i)
            (+ (ash (aref data (+ offset   (* i 4))) 24)
               (ash (aref data (+ offset 1 (* i 4))) 16)
               (ash (aref data (+ offset 2 (* i 4))) 8)
               (aref data (+ offset 3 (* i 4))))))
    (do ((i 16 (1+ i)))
        ((> i 79))
      (setf (aref message-block i)
            (to-32bit-word
             (rotl (logxor (aref message-block (- i 3))
                           (aref message-block (- i 8))
                           (aref message-block (- i 14))
                           (aref message-block (- i 16))) 1))))
    message-block))

(defun sha1-f (n x y z)
  (cond ((<= 0 n 19)
         (to-32bit-word (logior (logand x y)
                                (logand (lognot x) z))))
        ((or (<= 20 n 39) (<= 60 n 79))
         (to-32bit-word (logxor x y z)))
        ((<= 40 n 59)
         (to-32bit-word (logior (logand x y)
                                (logand x z)
                                (logand y z))))))

(defun sha1-k (n)
  (cond ((<=  0 n 19) #x5A827999)
        ((<= 20 n 39) #x6ED9EBA1)
        ((<= 40 n 59) #x8F1BBCDC)
        ((<= 60 n 79) #xCA62C1D6)))

(defun sha1 (message-vector)
  (let* ((h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (padded-message (pad-message message-vector))
         (n (/ (length padded-message) 64)))
    (dotimes (i n)
      (let ((a h0) (b h1) (c h2) (d h3) (e h4) (temp 0)
            (message-block (prepare-message-block i padded-message)))
        (dotimes (i 80)
          (setq temp (to-32bit-word (+ (rotl a 5)
                                       (sha1-f i b c d)
                                       e
                                       (sha1-k i)
                                       (aref message-block i))))
          (setq e d)
          (setq d c)
          (setq c (to-32bit-word (rotl b 30)))
          (setq b a)
          (setq a temp))
        (setq h0 (to-32bit-word (+ h0 a)))
        (setq h1 (to-32bit-word (+ h1 b)))
        (setq h2 (to-32bit-word (+ h2 c)))
        (setq h3 (to-32bit-word (+ h3 d)))
        (setq h4 (to-32bit-word (+ h4 e)))))
    (vector
     (logand #xFF (ash h0 -24))
     (logand #xFF (ash h0 -16))
     (logand #xFF (ash h0 -8))
     (logand #xFF h0)
     (logand #xFF (ash h1 -24))
     (logand #xFF (ash h1 -16))
     (logand #xFF (ash h1 -8))
     (logand #xFF h1)
     (logand #xFF (ash h2 -24))
     (logand #xFF (ash h2 -16))
     (logand #xFF (ash h2 -8))
     (logand #xFF h2)
     (logand #xFF (ash h3 -24))
     (logand #xFF (ash h3 -16))
     (logand #xFF (ash h3 -8))
     (logand #xFF h3)
     (logand #xFF (ash h4 -24))
     (logand #xFF (ash h4 -16))
     (logand #xFF (ash h4 -8))
     (logand #xFF h4))))
