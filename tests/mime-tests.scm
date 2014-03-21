
(import (chibi) (chibi mime) (chibi test))

(test-begin "mime")

(test '(text/html (charset . "UTF-8") (filename . "index.html"))
    (mime-parse-content-type "text/html; CHARSET=UTF-8; filename=index.html"))

(test '(multipart/form-data (boundary . "AaB03x"))
    (mime-parse-content-type "multipart/form-data, boundary=AaB03x"))

(test '(mime (@ (from . "\"Dr. Watson <guest@grimpen.moor>\"")
                (to . "\"Sherlock Homes <not-really@221B-baker.street>\"")
                (subject . "\"First Report\"")
                (content-type . "text/plain; charset=\"ISO-8859-1\""))
             "Moor is gloomy. Heard strange noise, attached.\n")
    (call-with-input-string
     "From:    \"Dr. Watson <guest@grimpen.moor>\"
To:      \"Sherlock Homes <not-really@221B-baker.street>\"
Subject: \"First Report\"
Content-Type: text/plain; charset=\"ISO-8859-1\"

Moor is gloomy. Heard strange noise, attached.

"
     mime-message->sxml))

;; from rfc 1867

(test '(mime
        (@ (content-type . "multipart/form-data, boundary=AaB03x"))
        (mime (@ (content-disposition . "form-data; name=\"field1\""))
              "Joe Blow")
        (mime (@ (content-disposition
                  . "form-data; name=\"pics\"; filename=\"file1.txt\"")
                 (content-type . "text/plain"))
              " ... contents of file1.txt ..."))
    (call-with-input-string
        "Content-type: multipart/form-data, boundary=AaB03x

--AaB03x
content-disposition: form-data; name=\"field1\"

Joe Blow
--AaB03x
content-disposition: form-data; name=\"pics\"; filename=\"file1.txt\"
Content-Type: text/plain

 ... contents of file1.txt ...
--AaB03x--
"
      mime-message->sxml))

(test '(mime
        (@ (content-type . "multipart/form-data, boundary=AaB03x"))
        (mime (@ (content-disposition . "form-data; name=\"field1\""))
              "Joe Blow")
        (mime (@ (content-disposition . "form-data; name=\"pics\"")
                 (content-type . "multipart/mixed, boundary=BbC04y"))
              (mime (@ (content-disposition
                        . "attachment; filename=\"file1.txt\"")
                       (content-type . "text/plain"))
                    "... contents of file1.txt ...")
              (mime (@ (content-disposition
                        . "attachment; filename=\"file2.gif\"")
                       (content-type . "image/gif")
                       (content-transfer-encoding . "binary"))
                    #u8(32 32 46 46 46 99 111 110 116 101 110
                        116 115 32 111 102 32 102 105 108 101
                        50 46 103 105 102 46 46 46))))
    (call-with-input-string
        "Content-type: multipart/form-data, boundary=AaB03x

--AaB03x
content-disposition: form-data; name=\"field1\"

Joe Blow
--AaB03x
content-disposition: form-data; name=\"pics\"
Content-type: multipart/mixed, boundary=BbC04y

--BbC04y
Content-disposition: attachment; filename=\"file1.txt\"
Content-Type: text/plain

... contents of file1.txt ...
--BbC04y
Content-disposition: attachment; filename=\"file2.gif\"
Content-type: image/gif
Content-Transfer-Encoding: binary

  ...contents of file2.gif...
--BbC04y--
--AaB03x--
"
      mime-message->sxml))

(test-end)
