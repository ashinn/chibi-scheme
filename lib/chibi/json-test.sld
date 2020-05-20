
(define-library (chibi json-test)
  (import (scheme base) (chibi json) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "json")
      (test 1 (parse-json "1"))
      (test 1.5 (parse-json "1.5"))
      (test 1000.0 (parse-json "1e3"))
      (test "á" (parse-json "\"\\u00e1\""))
      (test "𐐷" (parse-json "\"\\uD801\\uDC37\""))
      (test "😐" (parse-json "\"\\uD83D\\uDE10\""))
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
          (parse-json "{
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
          (parse-json "{\"menu\": {
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
      (test-end))))
