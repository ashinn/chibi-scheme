
(define-library (chibi json-test)
  (import (scheme base) (chibi json) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "json")
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
