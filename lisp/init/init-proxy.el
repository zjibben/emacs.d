;;
;; init-proxy
;;
;; initialize proxy for emacs internet access
;;
;; zjibben <threeofsix@gmail.com>

;; set proxy address and port
;; TODO: should be set outside this package
(defvar http-proxy-host  "proxyout.lanl.gov" "Host address for http proxy")
(defvar http-proxy-port  8080                "Host port for http proxy")
(defvar no-proxy-domain  "lanl\\.gov"        "Domain for which no proxy is needed")
(defvar https-proxy-host http-proxy-host     "Host address for https proxy")
(defvar https-proxy-port http-proxy-port     "Host port for https proxy")

(setq url-proxy-services 
      `(("no_proxy" . no-proxy-domain)
        ("http"  . ,(concat http-proxy-host  ":" (number-to-string http-proxy-port)))
        ("https" . ,(concat https-proxy-host ":" (number-to-string https-proxy-port))) ))

;; use function from http://www.emacswiki.org/emacs/ErcProxy for telling erc to use the proxy
(defun open-http-proxy-stream (name buffer host service &rest parameters)
  "Open network stream via http proxy.
Proxy is defined by variables http-proxy-host and http-proxy-port."
  (let ((tmp-process
         (apply 'open-network-stream name buffer http-proxy-host http-proxy-port parameters)))
    (process-send-string name (format "CONNECT %s:%d HTTP/1.1\n\n" host service))
    tmp-process))

(setq erc-server-connect-function 'open-http-proxy-stream)

(provide 'init-proxy)
