;;; azure-aks-test.el --- Tests for azure aks.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-aks)


(describe "Azure Aks"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure aks)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-aks-list-response.json")
                                    'azure-aks)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-aks (("name" "gnuherd")
                                                         ("id" "/subscriptions/deadbeef-beed-bade-b70f-ee9206029c60/resourcegroups/gnuherd/providers/Microsoft.ContainerService/managedClusters/gnuherd")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-aks) :to-be 1))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(aks))
                            (expect (s-trim  "azure aks
name        kubernetesVersion     location          size     provisioningState     
gnuherd     1.10.6                australiaeast     1        Succeeded") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure aks)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-aks)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-aks) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(aks))
                            (expect (s-trim  "azure aks
No aks found") :to-match (s-trim (substring-no-properties (buffer-string))))))))

          (describe "when scaled up"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure aks)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-aks-list-response.json")
                                    'azure-aks))

                                 (spy-on 'ecloud-read-int :and-return-value 10)
                                 (spy-on 'magit-confirm :and-return-value t)
                                 (spy-on 'ecloud-run-json-command))

                    (it "should call cli to scale up"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(aks))
                            (goto-line 3)
                            (call-interactively 'azure-aks-scale)
                            (expect 'ecloud-run-json-command :to-have-been-called-with '("az" "aks" "scale" "--name" "gnuherd" "--resource-group" "gnuherd" "--node-count" "10") nil nil)
                            ))))
          )

