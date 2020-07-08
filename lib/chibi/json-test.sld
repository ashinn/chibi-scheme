
(define-library (chibi json-test)
  (import (scheme base) (chibi json) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "json")
      (test-begin "string->json")
      (test 1 (string->json "1"))
      (test 1.5 (string->json "1.5"))
      (test 1000.0 (string->json "1e3"))
      (test "á" (string->json "\"\\u00e1\""))
      (test "𐐷" (string->json "\"\\uD801\\uDC37\""))
      (test "😐" (string->json "\"\\uD83D\\uDE10\""))
      (test '((glossary
               (title . "example glossary")
               (GlossDiv
                (title . "S")
                (GlossList
                 (GlossEntry
                  (ID . "SGML")
                  (SortAs . "SGML")
                  (GlossTerm . "Standard Generalized Markup Language")
                  (Acronym . "SGML")
                  (Abbrev . "ISO 8879:1986")
                  (GlossDef
                   (para . "A meta-markup language, used to create markup languages such as DocBook.")
                   (GlossSeeAlso . #("GML" "XML")))
                  (GlossSee . "markup"))))))
          (string->json "{
    \"glossary\": {
        \"title\": \"example glossary\",
		\"GlossDiv\": {
            \"title\": \"S\",
			\"GlossList\": {
                \"GlossEntry\": {
                    \"ID\": \"SGML\",
					\"SortAs\": \"SGML\",
					\"GlossTerm\": \"Standard Generalized Markup Language\",
					\"Acronym\": \"SGML\",
					\"Abbrev\": \"ISO 8879:1986\",
					\"GlossDef\": {
                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
						\"GlossSeeAlso\": [\"GML\", \"XML\"]
                    },
					\"GlossSee\": \"markup\"
                }
            }
        }
    }
}"))
      (test '((menu
               (id . "file")
               (value . "File")
               (popup
                (menuitem
                 . #(((value . "New") (onclick . "CreateNewDoc()"))
                     ((value . "Open") (onclick . "OpenDoc()"))
                     ((value . "Close") (onclick . "CloseDoc()")))))))
          (string->json "{\"menu\": {
  \"id\": \"file\",
  \"value\": \"File\",
  \"popup\": {
    \"menuitem\": [
      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},
      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},
      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}
    ]
  }
}}"))
      (test-end)
      (test-begin "json->string")
      (test "1" (json->string 1))
      (test "1.5" (json->string 1.5))
      (test "1000" (json->string 1E3))
      (test  "\"\\u00E1\"" (json->string "á"))
      (test  "\"\\uD801\\uDC37\"" (json->string "𐐷"))
      (test  "\"\\uD83D\\uDE10\"" (json->string "😐"))
      (test "{\"menu\":{\"id\":\"file\",\"value\":\"File\",\"popup\":{\"menuitem\":[{\"value\":\"New\",\"onclick\":\"CreateNewDoc()\"},{\"value\":\"Open\",\"onclick\":\"OpenDoc()\"},{\"value\":\"Close\",\"onclick\":\"CloseDoc()\"}]}}}"
       (json->string '((menu
                        (id . "file")
                        (value . "File")
                        (popup
                         (menuitem
                          . #(((value . "New") (onclick . "CreateNewDoc()"))
                              ((value . "Open") (onclick . "OpenDoc()"))
                              ((value . "Close") (onclick . "CloseDoc()")))))))))
      (test "{\"glossary\":{\"title\":\"example glossary\",\"GlossDiv\":{\"title\":\"S\",\"GlossList\":{\"GlossEntry\":{\"ID\":\"SGML\",\"SortAs\":\"SGML\",\"GlossTerm\":\"Standard Generalized Markup Language\",\"Acronym\":\"SGML\",\"Abbrev\":\"ISO 8879:1986\",\"GlossDef\":{\"para\":\"A meta-markup language, used to create markup languages such as DocBook.\",\"GlossSeeAlso\":[\"GML\",\"XML\"]},\"GlossSee\":\"markup\"}}}}}"
       (json->string '((glossary
               (title . "example glossary")
               (GlossDiv
                (title . "S")
                (GlossList
                 (GlossEntry
                  (ID . "SGML")
                  (SortAs . "SGML")
                  (GlossTerm . "Standard Generalized Markup Language")
                  (Acronym . "SGML")
                  (Abbrev . "ISO 8879:1986")
                  (GlossDef
                   (para . "A meta-markup language, used to create markup languages such as DocBook.")
                   (GlossSeeAlso . #("GML" "XML")))
                  (GlossSee . "markup"))))))))
      (test-end)
      (test-end)
      )))
