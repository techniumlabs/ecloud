# ecloud
[![Build Status](https://travis-ci.org/techniumlabs/ecloud.svg?branch=master)](https://travis-ci.org/techniumlabs/ecloud)
[![Coverage Status](https://coveralls.io/repos/github/techniumlabs/ecloud/badge.svg?branch=master)](https://coveralls.io/github/techniumlabs/ecloud?branch=master)
[![License GPL 3](https://img.shields.io/github/license/techniumlabs/ecloud.svg)](LICENSE)

A magit-style interface to access Azure, AWS and GCP. This project is in its infancy. 

## Installation
Right now only manual installation is supported. Will add the package to melpa soon.

## Manual Installation
To manually install ecloud, clone ecloud to a directory. Then add the following two lines to your .emacs file.

```
(add-to-list 'load-path "~/path/to/ecloud/")
(require 'ecloud)
```

If you are using spacemacs add the line to you .spacemacs file.

```
(setq dotspacemacs/additional/packages '(
    (ecloud :location (recipe :repo techniumlabs/ecloud :fetcher github))))
```

## Usage
Right now azure is what mainly supported. Other cloud is also worked on. For azure, run `M-x azure-overview` to get started.

## Contributing
You can contribute to this project in several ways. 

1. Raising Issues in github
2. Contributing code to fix an issue, implement any feature, improve tests
3. Reviewing the code and suggesting possible way to improve the elisp code. 

## Acknowledgements
This project would not have been possible if not for these exceptional projects 
1. [magit](https://github.com/magit/magit)
2. [kubernetes-el](https://github.com/chrisbarrett/kubernetes-el)

Thanks to all who contributed to the above projects and countless other emacs packages, without which emacs will not be a good place to live.

