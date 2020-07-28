
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
      (test-begin "make-json-reader")
      (let ()
        (define-record-type Employee
          (make-employee name id title department)
          employee?
          (name employee-name)
          (id employee-id)
          (title employee-title)
          (department employee-department))
        (define-record-type Team
          (make-team name lead devs)
          team?
          (name team-name)
          (lead team-lead)
          (devs team-devs))
        (define read-employee (make-json-reader Employee))
        (define read-team
          (make-json-reader
           `(,Team
             (lead . ,Employee)
             (name . ,string?)
             (devs . #(,Employee)))))
        (define (string->employee str)
          (read-employee (open-input-string str)))
        (define (string->team str)
          (read-team (open-input-string str)))
        (let ((emp1 (string->employee
                     "{\"name\": \"Bob\", \"id\": 3, \"title\": \"CEO\"}")))
          (test-assert (employee? emp1))
          (test "Bob" (employee-name emp1))
          (test 3 (employee-id emp1))
          (test "CEO" (employee-title emp1)))
        (test-assert (employee? (string->employee "{\"unknown\": \"foo\"}")))
        (test-error ((make-json-reader Employee #t)
                     (open-input-string "{\"unknown\": \"foo\"}")))
        (test-error (string->team "{\"name\": 3}"))
        (let ((team1 (string->team
                      "{\"name\": \"Tiger Cats\", \"lead\": {\"name\": \"House\", \"id\": 321}, \"devs\": [{\"name\": \"Cameron\", \"id\": 7}, {\"name\": \"Thirteen\", \"id\": 13}]}")))
          (test-assert (team? team1))
          (test-assert (employee? (team-lead team1)))
          (test "House" (employee-name (team-lead team1)))
          (test-assert (vector? (team-devs team1)))
          (test 2 (vector-length (team-devs team1)))
          (test "Cameron" (employee-name (vector-ref (team-devs team1) 0)))
          (test "Thirteen" (employee-name (vector-ref (team-devs team1) 1)))))
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

