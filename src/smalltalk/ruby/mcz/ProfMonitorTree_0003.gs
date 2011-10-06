
set class ProfMonitorTree
category: '*maglev-Ruby support'
classmethod:
monitorIntervalNs: ns block: aBlock

 ^ self monitorBlock: aBlock intervalNs: ns
%

