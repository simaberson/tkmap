'open ens.ctl'
'set grads off'
fname='points'
fname2='hurrloc'
fname3='stnlist0012'
fname4='stnlist00'
fname5='stnlist12'
'set mpdset hires'
i=1
nst = 0
while (1)
  res = read(fname)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of points file'
      say nst' flights to be plotted'
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'fname; endif
    if(rc = 8); say 'rc=8: 'fname' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'fname; endif
    return 99
  endif
  nst = nst + 1
  _points.nst = sublin(res,2)
endwhile
nsth = 0
while (1)
  res = read(fname2)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'fname2; endif
    if(rc = 8); say 'rc=8: 'fname2' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'fname2; endif
    return 99
  endif
  nsth = nsth + 1
  _hurrloc.nsth = sublin(res,2)
endwhile
rc = close(fname)
rc = close(fname2)
rc = getbounds(nst)
'set lat '_southlat' '_northlat
'set lon '_westlon' '_eastlon
'set cmin 0'
'set cmax 0'
'd pressfc'
'set line 3 1 6'
ist = 1
while(ist<=nst)
  i = 0
  while(1)
  ret=read(_points.ist)
  rc = sublin(ret,1)
  if(rc = 2)
    break
  endif
  rec = sublin(ret,2)
  xlatll=subwrd(rec,1)
  xlonll=-subwrd(rec,2)
  turn=substr(rec,18,1)
  'query ll2xy 'xlonll' 'xlatll
  xdum=sublin(result,1)
  xlonxy=subwrd(xdum,1)
  xlatxy=subwrd(xdum,2)
  if(turn != T)
    if(i > 0)
      'draw mark 3 'xlonxy' 'xlatxy' 0.175'
      'set strsiz 0.15 0.15'
      'draw string 'xlonxy+0.08' 'xlatxy+0.08' 'i
      'draw line 'xlonold' 'xlatold' 'xlonxy' 'xlatxy
    endif
  else
    if(i = 0)
      xlonold=xlonxy
      xlatold=xlatxy
    endif
    'draw line 'xlonold' 'xlatold' 'xlonxy' 'xlatxy
    i=i-1
  endif
  xlonold=xlonxy
  xlatold=xlatxy
  i=i+1
  endwhile
  ist = ist + 1
endwhile
xdum=sublin(result,1)
leglon=subwrd(xdum,1)
leglat=subwrd(xdum,2)
modstring='Dropwindsonde'
'query ll2xy '_westlon' '_southlat
'set string 3'
ist = 1
while (ist <= nst)
  ret=read(_hurrloc.ist)
  rec = sublin(ret,2)
  xlatll=subwrd(rec,1)
  xlonll=subwrd(rec,2)
  'query ll2xy 'xlonll' 'xlatll
  xdum=sublin(result,1)
  xlonxy=subwrd(xdum,1)
  xlatxy=subwrd(xdum,2)
  'draw wxsym 41 'xlonxy' 'xlatxy' 0.630'
  ist = ist + 1
endwhile
'set line 2'
while (1)
  ret=read(fname3)
  rc = sublin(ret,1)
    if(rc = 2)
      break
    endif
  rec = sublin(ret,2)
  xlatll=subwrd(rec,1)
  xlonll=subwrd(rec,2)
  if (xlonll <= _eastlon)
    if(xlonll >= _westlon)
      if(xlatll <= _northlat)
        if(xlatll >= _southlat)
          'query ll2xy 'xlonll' 'xlatll
          xdum=sublin(result,1)
          xlonxy=subwrd(xdum,1)
          xlatxy=subwrd(xdum,2)
          'draw mark 3 'xlonxy' 'xlatxy' 0.175'
        endif
      endif
    endif
  endif
endwhile
'query ll2xy '_westlon' '_southlat
xdum=sublin(result,1)
leglon=subwrd(xdum,1)
leglat=subwrd(xdum,2)
modstring='00 and 12 UTC Rawinsonde'
'set string 2'
'draw mark 3 'leglon+0.2' 'leglat+0.4' 0.150'
'draw string 'leglon+0.35' 'leglat+0.35' 'modstring
'set line 5'
while (1)
  ret=read(fname4)
  rc = sublin(ret,1)
    if(rc = 2)
      break
    endif
  rec = sublin(ret,2)
  xlatll=subwrd(rec,1)
  xlonll=subwrd(rec,2)
  if (xlonll <= _eastlon)
    if(xlonll >= _westlon)
      if(xlatll <= _northlat)
        if(xlatll >= _southlat)
          'query ll2xy 'xlonll' 'xlatll
          xdum=sublin(result,1)
          xlonxy=subwrd(xdum,1)
          xlatxy=subwrd(xdum,2)
          set ccolor 3
          'draw mark 3 'xlonxy' 'xlatxy' 0.175'
        endif
      endif
    endif
  endif
endwhile
'query ll2xy '_westlon' '_southlat
xdum=sublin(result,1)
leglon=subwrd(xdum,1)
leglat=subwrd(xdum,2)
modstring='00 UTC Rawinsonde'
'set string 5'
'draw mark 3 'leglon+0.2' 'leglat+0.6' 0.150'
'draw string 'leglon+0.35' 'leglat+0.55' 'modstring
'set line 9'
while (1)
  ret=read(fname5)
  rc = sublin(ret,1)
    if(rc = 2)
      break
    endif
  rec = sublin(ret,2)
  xlatll=subwrd(rec,1)
  xlonll=subwrd(rec,2)
  if (xlonll <= _eastlon)
    if(xlonll >= _westlon)
      if(xlatll <= _northlat)
        if(xlatll >= _southlat)
          'query ll2xy 'xlonll' 'xlatll
          xdum=sublin(result,1)
          xlonxy=subwrd(xdum,1)
          xlatxy=subwrd(xdum,2)
          'draw mark 3 'xlonxy' 'xlatxy' 0.175'
        endif
      endif
    endif
  endif
endwhile
'query ll2xy '_westlon' '_southlat
xdum=sublin(result,1)
leglon=subwrd(xdum,1)
leglat=subwrd(xdum,2)
modstring='12 UTC Rawinsonde'
'set string 9'
'draw mark 3 'leglon+0.2' 'leglat+0.8' 0.150'
'draw string 'leglon+0.35' 'leglat+0.75' 'modstring
'set line 8'
'gxprint ftk.png png white'
*---------------------------
*
*---------------------------
function getbounds(nst)

* This function looks through the forecast tracks points
* and/or the verification track points and figures out
* which points are the farthest N,E,S and W.  Remember,
* we are using degrees positive westward, the TPC standard.

_northlat =  -99.0
_southlat =   99.0
_westlon  =  361.0
_eastlon  = -361.0
ist = 1
while(ist<=nst)
  while(1)
  res=read(_points.ist)
  rc = sublin(res,1)
  if(rc = 2)
    break
  endif
  rec = sublin(res,2)
  ylat=subwrd(rec,1)
  xlon=-subwrd(rec,2)
  if (ylat > _northlat)
    _northlat = ylat
  endif
  if (ylat < _southlat)
    _southlat = ylat
  endif
  if (xlon < _westlon)
    _westlon = xlon
  endif
  if (xlon > _eastlon)
    _eastlon = xlon
  endif
  endwhile
  res=close(_points.ist)
  ist = ist + 1
endwhile
_northlat = _northlat + 5
_southlat = _southlat - 5
_westlon  = _westlon  - 5
_eastlon  = _eastlon  + 5
say 'At end of getbounds....'
say '  northlat = '_northlat'  southlat = '_southlat
say '  westlon  = '_westlon'   eastlon  = '_eastlon
_origwlon = _westlon
_origelon = _eastlon
_orignlat = _northlat
_origslat = _southlat
return
