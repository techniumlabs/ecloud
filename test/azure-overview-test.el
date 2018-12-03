;;; azure-overview-test.el --- Tests for azure overview.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-overview)

(describe "Azure Overview"
          (describe "When triggered"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure account)
                                   (ecloud-define-resource-model azure vnet)
                                   (ecloud-define-resource-model azure vm)
                                   (ecloud-define-resource-model azure aks)
                                   (ecloud-define-resource-model azure acr)
                                   (ecloud-define-resource-model azure aci)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-account-list-response.json")
                                    'azure-account)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-vnet-list-response.json")
                                    'azure-vnet)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-aks-list-response.json")
                                    'azure-aks)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-acr-list-response.json")
                                    'azure-acr)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-aci-list-response.json")
                                    'azure-aci)))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (run-hooks 'azure-overview-sections-hook)
                            (expect (s-trim  "Azure Cloud

azure account
name                            state       isDefault     
Technium Labs Australia         Enabled     true          

Cloud Name: AzureCloud
Tenant Id: deadbeef-2faa-4942-aa9e-deadbeef6a63
Username: technium.ecloud@example.com
User Type: user

Microsoft Azure Sponsorship     Enabled     false         

Cloud Name: AzureCloud
Tenant Id: aaaabbc7-dead-beef-aa9e-e26ab8bc6a63
Username: ecloud-user@example.com
User Type: user


azure vnet
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
aks-subnet     10.240.0.0/16     


azure vm
No vm found

azure aks
name        kubernetesVersion     location          size     provisioningState     
gnuherd     1.10.6                australiaeast     1        Succeeded             

azure acr
name             loginServer                 location       
techniumlabs     techniumlabs.azurecr.io     westeurope     

azure aci
name                              osType     resourceGroup     location        
wordpress-containerinstance       Linux      aci-test-rg       West Europe     
createshare-containerinstance     Linux      aci-test-rg       West Europe     

No Errors") :to-match
(s-trim (substring-no-properties (buffer-string)))))))
                    )
          )

