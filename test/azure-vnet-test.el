;;; azure-vnet-test.el --- Tests for azure vnet.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-vnet)


(describe "Azure Vnet"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure vnet)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-vnet-list-response.json")
                                    'azure-vnet)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-vnet (("name" "aks-vnet-deadbeef")
                                                               ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-deadbeef9c60/resourceGroups/gnuherd/providers/Microsoft.Network/virtualNetworks/aks-vnet-deadbeef")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-vnet) :to-be 7))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(vnet))
                            (expect (s-trim "azure vnet
name                   address-list     location          
myVMVNET               10.0.0.0/16      australiaeast     
azure subnet
name           addressPrefix     
myVMSubnet     10.0.0.0/24       

vnodes-vnet            10.0.0.0/8       westeurope        
azure subnet
name                 addressPrefix     
virtual-node-aci     10.241.0.0/16     
default              10.240.0.0/16     

vnode2-vnet            10.0.0.0/8       westeurope        
azure subnet
name                 addressPrefix     
virtual-node-aci     10.241.0.0/16     
default              10.240.0.0/16     

vnode-preview-vnet     10.0.0.0/8       westeurope        
azure subnet
name                 addressPrefix     
virtual-node-aci     10.241.0.0/16     
default              10.240.0.0/16     

pegasus2-vnet          10.0.0.0/8       westeurope        
azure subnet
name                 addressPrefix     
virtual-node-aci     10.241.0.0/16     
default              10.240.0.0/16     

aci-test               10.0.0.0/16      westeurope        
azure subnet
name           addressPrefix     
app-subnet     10.0.0.0/24       

aks-vnet-deadbeef      10.0.0.0/8       australiaeast     
azure subnet
name           addressPrefix     
aks-subnet     10.240.0.0/16") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure vnet)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-vnet)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-vnet) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(vnet))
                            (expect (s-trim  "azure vnet
No vnet found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )
