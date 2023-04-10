(require 'python-venv)
(require 'buttercup)

(describe "Get System python3 path"
  (it "Finds the system python3 executable and fetches it"
    (expect (python-venv-get-system-python) :to-equal "/usr/bin/python3")))


;; TODO: Add more tests
