'reinit'
'openall.gs'
'set grid off'
'set grads off'
*'set xsize 360 480'
'set parea  0.9 10.5 0.05 8.0'
'set xlopts 1 5 0.18'
'set ylopts 1 5 0.18'
'set cthick 5'
'set clopts -1 -1 0.12'
'set clskip 3 5.0'
'set display color white'

'set lev 5'
'smin5 = min(s.3,t=1,t=586)'
'c'
*'scl 0 1 .05'
'scl 0 .1 .01'
'd smin5'
'print smin5m.eps'

'set lev 50'
'smin50 = min(s.3,t=1,t=586)'
'c'
*'scl 0 1 .05'
'scl 0 .1 .01'
'd smin50'
'print smin50m.eps'

'set lev 100'
'smin100 = min(s.3,t=1,t=586)'
'c'
*'scl 0 1 .05'
'scl 0 .1 .01'
'd smin100'
'print smin100m.eps'

'set lev 200'
'smin200 = min(s.3,t=1,t=586)'
'c'
*'scl 0 1 .05'
'scl 0 .1 .01'
'd smin200'
'print smin200m.eps'

'set lev 300'
'smin300 = min(s.3,t=1,t=586)'
'c'
*'scl 0 0.8 0.05'
'scl 0 0.08 0.005'
'd smin300'
'print smin300m.eps'

'set lev 400'
'smin400 = min(s.3,t=1,t=586)'
'c'
*'scl 0 0.6 0.05'
'scl 0 0.06 0.005'
'd smin400'
'print smin400m.eps'

'set lev 500'
'smin500 = min(s.3,t=1,t=586)'
'c'
*'scl 0 0.44 0.04'
'scl 0 0.044 0.004'
'd smin500'
'print smin500m.eps'

'set lev 750'
'smin750 = min(s.3,t=1,t=586)'
'c'
*'scl 0 0.22 0.01'
'scl 0 0.022 0.001'
'd smin750'
'print smin750m.eps'

'set lev 1000'
'smin1000 = min(s.3,t=1,t=586)'
'c'
'scl 0 0.01 0.001'
'd smin1000'
'print smin1000.eps'

'set lev 2000'
'smin2000 = min(s.3,t=1,t=586)'
'c'
'scl 0 0.005 0.0005'
'd smin2000'
'print smin2000m.eps'
