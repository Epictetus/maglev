
set class IPSocket
category: '*maglev-Ruby support'
method:
rubyAddress
| portNum hostName ipAddrStr prefix |

ipAddrStr := self address .
prefix := '::ffff:' .
(ipAddrStr at: 1 equals: prefix ) ifTrue:[ 
  "translate mapped IP4 to IP4 format"
  ipAddrStr :=  ipAddrStr copyFrom: prefix size + 1 to: ipAddrStr size
].
hostName := self class getHostNameByAddress: ipAddrStr .
portNum := self port .

^ { 'AF_INET' . portNum . hostName . ipAddrStr }
%


set class IPSocket
category: '*maglev-Ruby support'
method:
rubyPeerAddress

| portNum hostName ipAddrStr |

ipAddrStr := self peerAddress .
hostName := self class getHostNameByAddress: ipAddrStr .
portNum := self peerPort .

^ { 'AF_INET' . portNum . hostName . ipAddrStr }
%

