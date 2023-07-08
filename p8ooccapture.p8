pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
--p8ooccapture v0.4.0
--@shiftalow
--knutil_0.4
--@shiftalow

function tohex(v,d)
v=sub(tostr(tonum(v),1),3,6)
while v[1]=='0' and #v>(d or 0) do
v=sub(v,2)
end
return v
end
--
--function tohex(p,n)
--p=sub(tostr(tonum(p),1),3,6)
--while sub(p,1,1)=='0' do
--p=sub(p,2)
--end
--p=join(tbfill(0,(n or 0)-#p),'')..p
--return p
--end

function bpack(w,s,b,...)
return b and flr(0x.ffff<<add(w,deli(w,1))&b)<<s|bpack(w,s-w[1],...) or 0
end

function bunpack(b,s,w,...)
if w then
return flr(0x.ffff<<w&b>>>s),bunpack(b,s-(... or 0),...)
end
end

function replace(s,f,r,...)
local a,i='',1
while i<=#s do
if sub(s,i,i+#f-1)~=f then
a..=sub(s,i,i)
i+=1
else
a..=r or ''
i+=#f
end
end
return ... and replace(a,...) or a
end

function toc(v,p)
return flr(v/(p or 8))
end

--function join(d,s,...)
--return not s and '' or not ... and s or s..d..join(d,...)
--end
function join(d,s,...)
local a={...}
while a[1] do
s..=d..deli(a,1)
end
return s or ''
end

function msplit(s,d,...)
local t=split(s,d or ' ',false)
if ... then
for i,v in pairs(t) do
t[i]=msplit(v,...)
end
end
return t
end

function htd(b,n)
local a={}
tmap(split(b,n or 2),function(v)
add(a,tonum('0x'..v))
end)
return a
end

function cat(f,...)
foreach({...},function(s)
for k,v in pairs(s) do
if tonum(k) then
add(f,v)
else
f[k]=v
end
end
end)
return f
end

function comb(k,p)
local a={}
for i=1,#k do
a[k[i]]=p[i]
end
return a
end

function tbfill(v,s,e,...)
local t={}
for i=s,e do
t[i]=... and tbfill(v,...) or v
end
return t
end

function rceach(p,f)
p=_rfmt(p)
for y=p.y,p.ey do
for x=p.x,p.ex do
f(x,y,p)
end
end
end

function oprint(s,x,y,f,o,p)
 for v in all(split(p or '\+ff,\+gf,\+hf,\+fg,\+hg,\+fh,\+gh,\+hh,')) do
  ?v..s,x,y,v~='' and o or f
 end
end

function tmap(t,f)
for i,v in pairs(t) do
v=f(v,i)
if v~=nil then
t[i]=v
end
end
return t
end

function mkpal(f,t)
return comb(htd(f,1),htd(t,1))
end
function ecmkpal(v)
return tmap(v,function(v,i)
i,v=unpack(v)
return tmap(v,function(v)
return mkpal(_ENV[i],v)
end)
end)
end
function ecpalt(p)
for i,v in pairs(p) do
if v==0 then
palt(i,true)
end
end
end

function ttable(p)
	return count(p) and p
end

function inrng(...)
return mid(...)==...
end

function amid(c,a)
return mid(c,a,-a)
end

function htbl(ht,c)
	local t,k,rt={}
	ht,c=split(ht,'') or ht,c or 1
	while 1 do
		local p=ht[c]
		c+=1
		if p=='{' or p=='=' then
			rt,c=htbl(ht,c)
			if not k then
				add(t,rt)
			else
				t[k],k=p=='{' and rt or rt[1]
			end
		elseif not p or p=='}' or p==';' or p==' ' then
			if k=='false' then k=false
			elseif k=='nil' then k=nil
			else k=k=='true' or tonum(k) or k
			end
			add(t,k)
			rt,k=p and c or nil
			if p~=' ' then
				break
			end
		elseif p~="\n" then
			k=(k or '')..p
		end
	end
	return t,rt
end


_mkrs,_hovk,_mnb=htbl'x y w h ex ey r p'
,htbl'{x y}{x ey}{ex y}{ex ey}'
,htbl'con hov ud rs rf cs cf os of cam'
function _rfmt(p)
local x,y,w,h=unpack(ttable(p) or split(p,' ',true))
return comb(_mkrs,{x,y,w,h,x+w-1,y+h-1,w/2,p})
end

function exrect(p)
local o=_rfmt(p)
return cat(o,comb(_mnb,{
function(p,y)
if y then
return inrng(p,o.x,o.ex) and inrng(y,o.y,o.ey)
else
return o.con(p.x,p.y) and o.con(p.ex,p.ey)
end
end
,function(r,i)
local h
for i,v in pairs(_hovk) do
h=h or o.con(r[v[1]],r[v[2]])
end
return h or i==nil and r.hov(o,true)
end
,function(p,y,w,h)
return cat(
o,_rfmt((tonum(p) or not p) and {p or o.x,y or o.y,w or o.w,h or o.h} or p
))
end
,function(col,f)
local c=o.cam
f=(f or rect)(o.x-c.x,o.y-c.y,o.ex-c.x,o.ey-c.y,col)
return o
end
,function(col)
return o.rs(col,rectfill)
end
,function(col,f)
(f or circ)(o.x+o.r-o.cam.x,o.y+o.r-o.cam.y,o.w/2,col)
return o
end
,function(col)
return o.cs(col,circfill)
end
,function(col)
return o.rs(col,oval)
end
,function(col)
return o.rs(col,ovalfill)
end
,{x=0,y=0}
}))
end
-->8

-->8
--dmp
function dmp(v,q,s)
	if not s then
	 q,s,_dmpx,_dmpy="\f6","\n",0,-1
	end
	local p,t=s
	tmap(ttable(v) or {v},function(str,i)
		t=type(str)
		if ttable(str) then
			q,p=dmp(str,q..s..i.."{",s.." ")..s.."\f6}",s
		else
		 q..=join('',p,i
		 ,comb(msplit"number string boolean function nil"
		 ,msplit"\ff#\f6:\ff \fc$\f6:\fc \fe%\f6:\fe \fb*\f6:\fb \f2!\f6:\f2"
			)[t],tostr(str),"\f6 ")
			p=""
		end
	end)
	q..=t and "" or s.."\f2!\f6:\f2nil"
	::dmp::
	_update_buttons()
	if s=="\n" and not btnp'5' then
		flip()
		cls()
		?q,_dmpx*4,_dmpy*6
		_dmpx+=(_kd'0'-_kd'1')*(1+_kd(4)*16)
		_dmpy+=(_kd'2'-_kd'3')*(1+_kd(4)*16)
		goto dmp
	end
	return q
end

function _kd(d)
return tonum(btn(d))
end

function dbg(...)
if ...~=nil then
add(_dbgv,{...})
else
tmap(_dbgv,function(t,y)
oprint(join(' ',unpack(
tmap(ttable(t) or {t},function(v)
return tostr(v)
end))),0,122+(y-#_dbgv)*6,5,7)
--?join(' ',unpack(ttable(t) or {t})),0,122+(y-#_dbgv)*6,7
end)
_dbgv={}
end
end
dbg()
-->8
function _init()
sx,sy=0,0
scale=2
mest,cmest=0,0
cwidth,cheight=1,1
mwidth,mheight=1,1
mpx,mpy=0,0
customfont=false
scr=exrect'0 0 128 128'
menuitem(1,'copy code',function(v)
printh(join('',[[_oocspr={
"paste ooc strings"
}
function oocp(s,...)
?_oocspr[s],...
end
oocp(1,64,64)
]]),'@clip')
cmest=60
end)
--dmp(numchrreplace)
function menud()
	menuitem(2,'custom font:'..(customfont and 'on' or 'off'),function(v)
		customfont=not customfont
		if customfont then
			p,compressivefonts,cs=matrixscan(0,0,128,128)
			customchars={}
			tmap(compressivefonts,function(v,i)
				return v
			end)
			for i,v in pairs(cs) do
				add(customchars,v)
			end
			cls()
			?'initialize compressor...',0,0,6
			printchars(customchars)
			flip()
			stop()
			poke(0x5f55,0x0)--using spsheet
			cls()
			printchars(customchars)
			poke(0x5f55,0x60)--using screen
			memset(0x5600,0,800)
			local fwidth,fheight,lheight=8,8,8
			rceach('0 0 16 16',function(cx,cy)
				local b=0
				rceach({0,0,fwidth,fheight},function(x,y)
					b|=sget(cx*fwidth+x
										,cy*fheight+y)
										>0 and 1<<x or 0
					if x==fwidth-1 then
		--				?peek(0x5600+cx*8+cy*128+y)
		--				flip()
						
						poke(0x5600+cx*8+cy*128+y,b)
						b=0
					end
				end)
			end)
			poke(0x5600,fwidth,fwidth,lheight,0,0,0x3)
			local s=tmap({peek(0x5600,(#customchars+16)*8)},function(v)
				return controlcodes[v+1] or escapereplace(chr(v))
			end)
			printh(join('',[[poke(0x5600,ord("]]
		--	[[']],[[\']],[["]],[[\"]]
			,
--			replace(
			join('',unpack(tmap(s,function(v,i)
					return numreplace[(s[i-1] or '')..v] or v
				end))
--				),unpack(msplit'\\ \\\\ " \\" \0 \\0 \t \\t \n \\n \r \\r')
			)

--			replace(
--				replace(
--					chr(peek(0x5600,(#customchars+16)*8))
			--			,unpack(msplit'\\ \\\\ " \\" \0 \\0 \t \\t \n \\n \r \\r')
--					,unpack(msplit'\\ \\\\ " \\034 \0 \\000 \* \\* \# \\# \- \\- \| \\| \+ \\+ \^ \\^ \a \\a \b \\b \t \\t \n \\n \v \\v \f \\f \r \\r \14 \\014 \15 \\015')
--				),unpack(numchrreplace)
--			)
			,[[",1,0x800))]]),'@clip')
			reload(0x0,0x0,0x2000)
			mest=60
		else
			compressivefonts,customfont=nil
		end
		menud()
	end)
end
menud()
end

function _update()
dropfileds()
getmouse()
tmap(btnc,function(v,i)
local s=not scans[i] and btn(i) or stat(28,scans[i] or 0)
btns[i],v=s,s and v+1 or 0
butrg[i],btrg[i],btp[i]
=v<btnc[i],v==1,btnp(i)
return v
end)
_key=stat(31)
--_mx,_my=btns.ctr and (_mx or 0)+stat(38)/6 or mo.x,btns.ctr and (_my or 0)+stat(39)/6 or mo.y
--_mx,_my=btns.ctr and _ml and _mx or mo.x,btns.ctr and _ml and _my or mo.y
_mx,_my=(_mx or 0)+stat(38)/6,(_my or 0)+stat(39)/6
--dbg(_mx,_my)
mo.ex,mo.ey=_mx,_my
_amb,_mlt,_mrt,_mmt=ambtn()
_ml,_mr,_mm,_mw,_ismw=
mo.l,mo.r,mo.m,mo.w,mo.w~=0
_amd=_ml or _mr or _mm
lt,rt,ut,dt,zt,xt,et=unpack(btrg,0)
lp,rp,up,dp,zp,xp,ep=unpack(btp,0)
end

function _draw()
--	cls()
	fillp(0x5a5a)
	scr.rf(1)
	if not compressivefonts then
		?'initialize compressor...',0,0,6
		printchars()
		flip()
		poke(0x5f55,0x0)
		cls()
		printchars()
		poke(0x5f55,0x60)
		compressivefonts={}
		local p
		p,compressivefonts,customchars=matrixscan(0,0,128,128)
		customchars=nil
--		dmp(compressivefonts)
--		compressivefonts['◝◝◝◝◝◝◝◝']=[[\^i \^-i]]
		reload(0,0,0x2000)
	end

	poke(0x5f2d,5)
--	_mx,_my=mid(_mx,-8,135),mid(_my,-8,135)
	_mx,_my=mid(_mx,-32,160),mid(_my,-32,160)
	_mmx,_mmy=flr(_mx),flr(_my)
	if _mlt or _mrt or btrg[' '] then
		px,py=trunc(grid(sx)),trunc(grid(sy))
		cpx,cpy=cpx or 0,cpy or 0
		mpx,mpy=mpx or scale,mpy or scale
		_msx=_mmx
		_msy=_mmy
	end
	if cpx and _ml then
		if btns.ctr then
			mpx=mid(mpx+stat(38)/6,0,128)
			mpy=mid(mpy+stat(39)/6,0,128)
			mwidth=toc(mpx+cwidth*8,cwidth*8)
			mheight=toc(mpy+cheight*8,cheight*8)
--			dbg(mpx,cwidth*8,mwidth)
		else
			cpx=mid(cpx+stat(38)/6,0,128)
			cpy=mid(cpy+stat(39)/6,0,128)
			cwidth=mid(toc(cpx)+1,1,16)
			cheight=mid(toc(cpy)+1,1,16)
		end
		_mmx=_msx
		_mx=_msx
		_mmy=_msy
		_my=_msy
	end

	if _mr or btns[' '] then
		sx,sy=
		px+trunc(grid(_msx))-trunc(grid(_mmx))
		,py+trunc(grid(_msy))-trunc(grid(_mmy))
		sx,sy=amid(sx,96),amid(sy,96)
	end
	
	if mo.lut or mo.rut or butrg[' '] then
--		sx,sy=trunc(sx),trunc(sy)
--		_msx,_mmx=
--		trunc(sx-px+_mmx)
--		,trunc(sx-px+_msx)
--		,sy-py
	end
	
	if butrg.ctr then
		_mx=trunc(sgrid(_mmx,sx,1))
		_my=trunc(sgrid(_mmy,sy,1))
		_mmx=_mx
		_mmy=_my
	end
	if _ismw then
		pscale,scale=scale,mid(1,4,scale-_mw)
		sx,sy=flr((sx+_mmx)/pscale)*scale-_mmx
		,flr((sy+_mmy)/pscale)*scale-_mmy
	end

	camera(sx,sy)
	scr.ud(nil,nil,128*scale,128*scale).rf(0)
	camera()

	sspr(sx/scale,sy/scale,128,128,0,0,128*scale,128*scale)
	local scc=8*scale
	local fls=time()%0.4<0.2
	local rc=exrect'0 0 0 0'.ud(trunc(sgrid(_mmx,sx)),trunc(sgrid(_mmy,sy)),cwidth*8*scale,cheight*8*scale)
	local cr=exrect'0 0 0 0'
	fillp(0xcc33.8)
	rceach(mpx and {0,0,mwidth,mheight} or '0 0 0 0',function(x,y,r)
		if (x+y-toc(time(),0.06))%16<12 then
			cr.ud(
				rc.x+x*cwidth*scale*8
				,rc.y+y*cheight*scale*8
				,cwidth*scale*8+(x<r.ex and 0 or 0)
				,cheight*scale*8+(y<r.ey and 0 or 0)).rs(fls and 9 or 15)
		end
	end)
	fillp()
--	rc.rs(fls and 8 or 7)

--	local r=exrect'0 0 0 0'.ud('0 112 128 16').rf(0).rs(1)
	local r=exrect'0 0 0 0'.ud('0 112 128 16')

	local s={}
	for y=0,7 do
		local d=add(s,{})
		for x=7,0,-1 do
			add(d,sget(x+divide(trunc(sx)+sgrid(_mmx,sx)),y+divide(trunc(sy)+sgrid(_mmy,sy)))~=0 and 1 or 0)
--			add(d,sget(x+divide(trunc(sx)+grid(_mmx)),y+divide(trunc(sy)+grid(_mmy)))~=0 and 1 or 0)
		end
	end
	--escape
	local gescs=[[\^.]]
	tmap(s,function(v,i)
		v=bpack({1},7,unpack(v))
		gescs..=controlcodes[v+1] or chr(v)
		--dbg(v)
		return chr(v)
	end)
--	s=join('',unpack(s))
	local fc,sc=0,1
--	if _key=='る' then
	if btns.ctr and btrg.c then
		escs={}
		local sr=exrect({0,0,mwidth,mheight})
		rceach(sr.w*sr.h>0 and sr.p or '0 0 1 1',function(x,y,r)
			add(escs,[["]]..matrixscan(
				x*cwidth*8+divide(trunc(sx)+sgrid(_mmx,sx))
				,y*cheight*8+divide(trunc(sy)+sgrid(_mmy,sy))
				,cwidth*8,cheight*8,btns.sft
			)..[["]])
		end)
		escs=join("\n,",unpack(escs))
--		escs=matrixscan(divide(trunc(sx)+_mmx),divide(trunc(sy)+_mmy),16,16)
--		dmp(escs)
		printh(escs,'@clip')
		sc,fc=12,7
		cr.rf(fc).rs(sc)
		mest=60
	end

	rc.rs(fls and 8 or 7)
	if not _ml then
		r.rf(0).rs(1)
		if mest>0 then
		cr.ud('12 97 116 15').rf(fc).rs(sc)
		?join('',"*copied* please paste with\n          [PUNY FONT MODE]"),cr.x+2,cr.y+2,7
		mest-=1
		elseif cmest>0 then
		cr.ud('12 97 116 15').rf(fc).rs(sc)
		?join('',"*copied the drawing code*"),cr.x+2,cr.y+2,7
		cmest-=1
		else
		cr.ud('72 97 56 15').rf(fc).rs(sc)
		?join('','scr:X',pad0(sx),' Y',pad0(sy)),cr.ex-52,cr.y+2,6
		print(join(''
			,'pos:X'
			..pad0(toc(rc.x+sx,scale))
			..' Y'
			..pad0(rc.y+toc(sy,scale))
		),cr.ex-52,cr.y+8,6)
		end
		s=join('',unpack(s))
		?join('','esc:',escs),r.x+2,r.y+2
		?join('','raw:',s),r.x+2,r.y+8
		cr.ud('0 100 12 12').rf(fc).rs(sc)
		?join('',"\^.",s),cr.x+2,cr.y+2,7
	else
		if btns.ctr then
			?'○',rc.x+mpx*scale-1,rc.y+mpy*scale-2,fls and 9 or 7
		else
			?'○',rc.x+cpx*scale-1,rc.y+(cpy)*scale-2,fls and 8 or 7
--			?'○',(cpx-1/scale)*scale-sx,(cpy-2/scale)*scale-sy,fls and 8 or 7
		end
	
		oprint(join('','X',pad0(cwidth),'Y',pad0(cheight)),mid(rc.x,1,96),mid(rc.y-8,1,120),fls and 8 or 8,fls and 7 or 7)
	end
	dbg()
end

cat(_ENV,htbl[[
btns{}btrg{}butrg{}btnb{}btp{}
scans{}
]])
btnc=tbfill(0,0,7)
tmap(split("abcdefghijklmnopqrstuvwxyz1234567890\n?\b\t -✽[]\\?;'`,./","",false),function(v,i)
scans[v],btnc[v]=i+3,0
end)
scans.ctr=224
btnc.ctr=0
scans.sft=225
btnc.sft=0

poke(0x5f5c,8) --repeat start
poke(0x5f5d,2) --repeat distance
_most,_mobtn,_moky=unpack(htbl([[
{l=0;r=0;m=0;stx=0;sty=0;x=0;y=0;lh=0;rh=0;mh=0;}
{{l lt lut lh ldb} {r rt rut rh rdb} {m mt mut mh mdb}}
{x y l r m w sx sy lh rh mh vx vy}
]]))
poke(0x5f2d,1)
function ambtn()
return mo.lt or mo.rt or mo.mt,mo.lt,mo.rt,mo.mt
end
function getmouse()
local mb=stat(34)
mo=comb(_moky,
{stat(32)
,stat(33)
,mb&1>0
,mb&2>0
,mb&4>0
,stat(36)
,_most.stx
,_most.sty
,_most.lh
,_most.rh
,_most.mh
,mo and mo.x~=stat(32)
,mo and mo.y~=stat(33)
})

tmap(_mobtn,function(k)
local k,t,ut,h,db=unpack(k)
if mo[k] then
_most[k]+=1
mo[ut]=false
else
mo[ut],_most[k]=_most[k]>0,0
end
mo[t]=_most[k]==1
_most[h]=max(0,_most[h]-1)
if mo[t] then
mo[db]=_most[h]>0
_most[h]=mo[db] and 0 or 12
end
end)
if ambtn() then
_most.stx,_most.sty=mo.x,mo.y
end
mo.sx,mo.sy=_most.stx,_most.sty
mo.mv=(mo.x~=_most.x) or (mo.y~=_most.y)
_most.x,_most.y=mo.x,mo.y
return mo
end

function setmessage(t)
cmdscenes([[
ms st message ]]..#t*9,htbl(t))
end

-->8
function matrixscan(x,y,w,h,g)
	local r={x,y,w,h}
	local rc=cat({0,0,toc(w),toc(h)})
	local cmp,cstack,kstack,sbstack,jskstack={},{},{},{},{}
--	local sbs={}
--dmp(r)
	pal()
	local p={}
	tmap({peek(0x5f01,15)},function(c)
		rceach(r,function(...)
			p[c]=p[c] or sget(...)==c and c or nil
		end)
	end)
	--p={1,2,nil,nil,5,...}
--dmp(p)
	local str={}
	tmap(p,function(c,i)
--	dmp(c)
		local cs,cmcnt,zcnt,las={},{},0,''
		--rc={0,0,1,1}
		rceach(rc,function(xc,yc,rc)
			local sb=''
			local s=''
--			dmp(x+7-(x-xc*8))
			rceach({x,y,8,8},function(px,py,r)
				s..=tonum(sget(x+xc*8+r.ex-px,yc*8+py)==c)
--				s..=tonum(sget(x+r.ex-px,py)==c)
			end)
			--s="1111111111111111..."
			s=tmap(msplit(s,8),function(v)
				return tonum('0b00000000'..v)
			end)
--			dmp(s)
			--s={255,255,255,...}
			local vcnt=0
			s=tmap(s,function(v)
				sb=sb..chr(v)
				vcnt+=v
				return controlcodes[v+1] or escapereplace(chr(v))
			end)
			
			--zero count, reset if non-zero
			zcnt=vcnt~=0 and 0 or zcnt+1
			--s={"\0","\0",...}
			s=tmap(s,function(v,i)
--						if c==4 and las=="\\0" and  then
--			dmp({s[i-1],las})
--							dmp({cs[#cs],s[i-1]})
--						end
				return numreplace[(s[i-1] or '')..v] or v
			end)
			local js=join('',unpack(s))
			add(cstack,js)
			add(sbstack,sb)
--			if customchars and compressivefonts[js] then
--				poke(0x5f58,0x81)
--				?compressivefonts[js],c
--				poke(0x5f58,0)
--				?js..' \^.'..reversereplace(js),c
--				flip()
--			end
			local cm=compressivefonts[js] and escapereplace(compressivefonts[js]) or [[\^.]]..js
			add(cmcnt,numreplace[(sub(cmcnt[#cmcnt],-2) or '')..cm] or cm)
--			add(cmcnt,cm)
			local ise,c1=xc==rc.ex,cmcnt[1]
			local c1nmch=c1~=cm
			if ise or cmcnt[2] and c1nmch then
				local c1cnt=count(cmcnt,c1)
			 --"o-o-char" or "direct char"
				cm=c1cnt>(#c1>4 and 1 or 3)
					 and {[[\*]]..tohex(#cmcnt-1)..cmcnt[1]}
					 or {unpack(cmcnt,1,#cmcnt-1)}
				cat(cs,cm)
--			 if c==4 and #cs>100 and c1=="▮" and c1cnt>2 then
--			 dmp({c1cnt,cs,cm,cmcnt})
--			 end
				if ise then
					add(cs,cmcnt[#cmcnt])
					for i=1,c1cnt>3 and tonum(zcnt>0)*2-tonum(c1nmch) or zcnt do
						deli(cs)
					end
					add(cs,[[\n]])
					cmcnt={}
					zcnt=0
				else
					cmcnt={cmcnt[#cmcnt]}
				end
			end
		end)
		add(cs,not g and [[\f]]..tohex(c) or '',1)
		cs=join("",unpack(cs))
		add(str,cs)
--		add(str,join("",unpack(cs)))
	end)
--	cls()
	--** make to compressive fonts indexes
--	tmap(cstack,function(v,i)
--dmp({#cstack,#sbstack})
	tmap(sbstack,function(v,i)
		if count(kstack,v)==0 then
			if customfont then
				add(kstack,v)
				add(jskstack,cstack[i])
			elseif i>17 then 
				kstack[i-16]=v
				jskstack[i-16]=cstack[i]
--				kstack[i-16]=v
			end

--		elseif count(kstack,v)>1 then
--			?'\^x8\^y8\^.'..reversereplace(v)..' '..count(kstack,v)
--			flip()
--			stop()
		end
	end)

	local c,i,v=1
	while #kstack>240 do
		i,v=next(kstack,i)
--		if count(sbstack,v)>=2 then
--			dmp({count(sbstack,v),v})
--		end
		if count(sbstack,v)<=c then
--		if count(cstack,v)<=c then
			deli(kstack,i)
			deli(jskstack,i)
			if i then
				i-=1
			end
		end
		if i==nil then
			c+=1
		end
	end
--dmp(kstack)
--dmp(jskstack)
--	tmap(kstack,function(v,i)
	tmap(jskstack,function(v,i)
		cmp[v]=chr(i+15)
--		.."\n"
	end)
--dmp({#cstack,#sbstack})
--		dmp(cmp)

	return [[\^x8\^y8]]..join([[\^g]],unpack(str)),cmp,kstack
end

-->8

--for scroll
function grid(g)
return btns.ctr and toc(g,8*scale)*8*scale or g
end

--for mouse cursor
function sgrid(g,s,f)
return (btns.ctr or f)
 and toc(g+s%(scale*8),8*scale)*8*scale-s%(8*scale)
-- and toc(g,8*scale)*8*scale-s%(8*scale)
  or g
end

function trunc(d,s)
--return d
return d-d%(s or scale)
end

function divide(d,s)
return flr((d)/(s or scale))
end

function pad0(s,n)
return (s>=0 and '' or '-')..sub(('00')..abs(s),(n or -3)+(s>=0 and 0 or 1))
--return sub('  '..s,n or -3)
end

function dropfileds()
	local s=''
	while stat(121) do
		serial(0x802,0x8000,0x4004)
		for i=0,0x1fff do
			local p,q=peek(0x8004+i*2,2)
			poke(i,p+(q<<4))
--			if i%128==0 then flip() end
		end
		cstore()
	end
	if s~='' then
		s=htbl(replace(s,"\r",""))
		customscales=s
		customscaleupd()
		cmdscenes([[
				u us nil 0
				k us nil 0
				m us nil 0
				t us nil 0
				s us nil 0
				r us typename 0
			]],cat(htbl"f=saveall; n=save	all	data?(type:yes);len=3;d=\t;"
		,{
		fn=function()
			if typestr.allsave=='yes' then
				for i=1,16 do
					poke4(0x5e00+i*8-8
					,customscalepack(i))
				end
				cstore(0x5e00,0x5e00,0x100)
			end
		end
		,esfn=typingescfn
		}))
	end
end

function printchars(a)
--	if a then
--	poke(0x5f58,0x81)
--	end
	if a then
--	dmp(compressivefonts)
--	dmp(a[215-15])
		for i=16,255 do
--\0 \* \# \- \| \+ \^ \a
-- \b \t \n \v \f \r \014 \015
			if a[i-15] then
--				local p=reversereplace(a[i-15])
				local p=a[i-15]
				print("\^x8\^y8\^."..p,i%16*8,toc(i,16)*8,15)
--				print("\#"..tohex((i%15),1).."\^x8\^y8\^."..p,i%16*8,toc(i,16)*8,15)
--				rectfill(0,122,128,128,0)
--				?join(' ',"\f6\^g"..i,chr(i),p),0,122,7
--				while 1 do
--					_update_buttons()
--					if btnp(4) then
--					break
--					end
--					flip()
--				end
			end
		end
--		stop()
	else
		for i=16,255 do
			print(chr(i),i%16*8,toc(i,16)*8,6)
--			print(chr(i),i%16*8,toc(i,16)*8-8,6)
		end
	end
	poke(0x5f58,0)
end

--controlcodes{
--\0 ¹ ² ³ ⁴ ⁵ ⁶ ⁷
-- ⁸ \t \n ᵇ ᶜ \r ᵉ ᶠ
--}
cat(_ENV,htbl[[

controlcodes{
\0 \* \# \- \| \+ \^ \a
 \b \t \n \v \f \r \014 \015
}
numreplace{
\00=\048;
\01=\049;
\02=\050;
\03=\051;
\04=\052;
\05=\053;
\06=\054;
\07=\055;
\08=\056;
\09=\057;
}
]]
)
--numchrreplace{
--\00 \0000
-- \01 \0001
-- \02 \0002
-- \03 \0003
-- \04 \0004
-- \05 \0005
-- \06 \0006
-- \07 \0007
-- \08 \0008
-- \09 \0009
--}
--\00 \0\048
-- \01 \0\049
-- \02 \0\050
-- \03 \0\051
-- \04 \0\052
-- \05 \0\053
-- \06 \0\054
-- \07 \0\055
-- \08 \0\056
-- \09 \0\057
--compressivefonts=
--comb({
--		[[\000\000\000\000\000\000\000\000]]
--	},{
--		' '
--})
--dmp(compressivefonts)
--dmp(comb(split('0 4',' ',false),{' ','1'}))
--dmp(cat(htbl[[]],comb(split('0 4',' ',false),{' ','1'})))

--function reversereplace(s)
--return replace(
--s
--,[[\\]],[[\]]
--,[[\']],[[']]
--,[[\"]],[["]]
--
--,[[\048]],[[\00]]
--,[[\049]],[[\01]]
--,[[\050]],[[\02]]
--,[[\051]],[[\03]]
--,[[\052]],[[\04]]
--,[[\053]],[[\05]]
--,[[\054]],[[\06]]
--,[[\055]],[[\07]]
--,[[\056]],[[\08]]
--,[[\057]],[[\09]]
--
--,"\\*","\*"
--,"\\#","\#"
--,"\\-","\-"
--,"\\|","\|"
--,"\\+","\+"
--,"\\^","\^"
--,"\\a","\a"
--,"\\b","\b"
--,"\\t","\t"
--,"\\n","\n"
--,"\\v","\v"
--,"\\f","\f"
--,"\\r","\r"
--,"\\014","\014"
--,"\\015","\015"
--,"\\0","\0"
--)
--end
--numreplace{

--}
--function reversereplace(s)
--return replace(
--s
--,"\\t","\t"
--,"\\n","\n"
--,"\\r","\r"
--,"\\0","\0"
--)
--end

function escapereplace(s)
return replace(
s
,[[\]],[[\\]],[[']],[[\']],[["]],[[\"]]
)
end
-->8
-- control method
--[[
mouse moving:
	move cursor

ctrl/cmd key + mouse moving:
	grid movement
	
left button drag:
	capture size change

ctrl/cmd + left button drag:
	change multiple selections

right button drag (space-bar):
	scroll screen

ctrl/cmd + right button drag (space-bar):
	screen grid scrolling

mouse wheel:
	zoom in/out of screen

ctrl/cmd + c key
	one-off character captured in full color

ctrl/cmd + shift + c key
	capture one-off character in silhouette

enter
	open the pause menu
	(the sample code for drawing is in this.)

**when printing a capture, it is recommended to specify coordinates.**
]]--
-->8
--[[
release note
**v0.4.0**
- add: support for repeating instructions ("\*x") for duplicate patterns.
- add: delete whitespace before line break.
- add: [ui]color-coded sprite sheet range.
- add: [ui]compressor initialization with message.
- add: [ui]displays a guide cursor for range selection and multiple selections.
- add: [ui]adjust the scroll range of the sprite sheet.
- fix: [ui]adjustment of range and multiple selections.
- fix: [ui]hide ui during selection operation.
- fix: changed position display to 0-padding.
- del: eliminated compression of inverted fills.

**v0.3.0**
- add: support for single-character expressions by matching against default font patterns.
- fix: substitution to split character codes 0~15 and numeric characters.
- add: the most frequent patterns are selected as shortened characters. (default font supported)
- add: instruction to turn off border background.

**v0.2.0**
- add: supports grid movement.
- add: sprite capture size change.
- add: captures of multiple sprites at once.
- add: support for drag-and-drop import of sprites.
- add: copy of code for simple drawing to menu.
- fix: conversion method of control codes "\0 \14 \15".
- fix: add double quotation marks to the capture code

**v0.1.0**
- first release.
]]--

__gfx__
8888888888899999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888889999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888899999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888889999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888889999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888889999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888889999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888899999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888889999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888889999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888899999999999999999aaaaaaaaaaaaaaaaaaa0000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888899999999999999999aaaaaaaaaaaaaaa0000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888999999999999999999aaaaaaaaaaaa0000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888999999999999999999aaaaaaaaaa0000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888899999999999999999aaaaaaaa0000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888999999999999999999aaaaa0000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888999999999999999999aaaa00000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888889999999999999999999aa000000000000000000000000000aaaaaaa000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888889999999999999999999000000000000000000000000000000aaaa00000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888999999999999999990000000000000000000000000000000aaa00000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888999999999999999900000000000000000000000000000000a000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888899999999999999900000000000000000000000000000000000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888889999999999999900000000000000000000000000000000000000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888899999999999900000000000aaaaaaaaaa0000000000000000000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888999999999999000000000aaaaaaaaaaaaaa000000000000000000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888999999999990000000099aaaaaaaaaaaaaa0000000000000000000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888999999999900000009999aaaaaaaaa0000000000000000000000000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888999999990000009999999aaaaaa00000000000000000000000000000aaaaaaaa000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888999999990000099999999aaaa00000000000000000000000000000000aaaaaaaaaa0000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888889999999000099999999999a0000000000000000000000000000000000000aaaaaaaaa000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888899999990009999999999990000000000000000000000000000000000000000aaaaaaaaa0000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888899999000999999999999000000000000000000000000000000000000000000aaaaaaaaa0000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888999990999999999999900000000000000000000000000000000000000000000aaaaaaaaaa000000aaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888899999999999999999900000000000000000000000000000000000000000000000aaaaaaaaa00000aaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888999999999999999900000000000aa000000000000000000000000000000000000aaaaaaaaa00000aaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888889999999999999990000000000aaa00000000000000000000000000000000000000aaaaaaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888889999999999999900000000099aa0000000000000000000000000000a00000000000aaaaaaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888999999999999900000000999a000000000000000000000000000000aa000000000aaaaaaaaaaa00aaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888899999999999900000009999900000000000000000000000000000000aaa00000000aaaaaaaaaaa0aaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888899999999990000000999990000000000000000000000000000000000aaa00000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888899999999990000009999990000000000000000000000000000000000aaaa0000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888889999999990000009999990000000aa000000000000000000000000000aaaa0000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888999999990000099999900000009a000000000000000000000a0000000aaaa0000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888889999999000009999999000000999000000000000000000000aa000000aaaaa000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888889999990000999999900000009900000000000000000000000a0000000aaaaa000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888889999900009999999900000099900000000000000000000000aa000000aaaaaa00000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888999900099999999000000999000000a00000a00000a00000aaa000000aaaaaa00000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888999900099999999000009999000009a00000a00000a000000aaa00000aaaaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888899000999999990000009990000009900000a00000aa00000aaa000000aaaaaa00000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888899009999999990000099990000009900000a00000aa00000aaaa00000aaaaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888889099999999900000999990000099900000a00000aaa00000aaa000000aaaaaaa000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888888888899999999990000099990000009900000aa00000aaa00000aaaa00000aaaaaaaa00aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888888999999999900009999900000999000009aa0000aaa00000aaaaa0000aaaaaaaa000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888999999990000999990000009990000099a00000aaa0000aaaaa00000aaaaaaaa00aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888899999990000999900000099990000099900000aaa00000aaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888899999990009999900000099990000099900000aaa00000aaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888889999900009999900000099990000099900000aaaa0000aaaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888888888888889999000999990000009999900009999000009aaa0000aaaaaa0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888899900099900090000999990000999900000999a0000aaaaaaa000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888888888888990099900099000099999000099990000099990000aaaaaaa000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888909999900090009999990000999990000999990000aaaaaaa00aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888899999900000009999990000999990000999990000aaaaaaa00aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888888888888888999900090009999999000999999000099999000099aaaaaa0aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888888888888888888889000999000999999900099999900009999990009999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888888888888888888880009999000999999900099999900099999990009999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888888000099900999999990009999990009999999000999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888888000099900999999990009999990009999999900999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888880000889909999999990099999999009999999900999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888800008888899999999990099999999009999999990999999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
88888888888888888888888888888888888880000888888899999999900999999990099999999999999999999999999999999aaaaaaaaaaaaaaaaaaaaaaaaaaa
888888888888888888888888888888888888800008888888889999999099999999900999999999999999999999999999999999999aaaaaaaaaaaaaaaaaaaaaaa
8888888888888888888888888888888888888000088888888888999999999999999999999999999999999999999999999999999999999aaaaaaaaaaaaaaaaaaa
88888888888888888888888888888888888800000888888888888899999999999999999999999999999999999999999999999999999999999aaaaaaaaaaaaaaa
8888888888888888888888888888888888800008888888888888888899999999999999999999999999999999999999999999999999999999999999aaaaaaaaaa
888888888888888888888888888888888000008888888888888888888889999999999999999999999999999999999999999999999999999999999999999aaaaa
88888888888888888888888888888888800000088888888888888888888889999999999999999999999999999999999999999999999999999999999999999999
88888888888888888888888888888888880000088888888888888888888888889999999999999999999999999999999999999999999999999999999999999999
88888888888888888888888888888888800000088888888888888888888888888889999999999999999999999999999999999999999999999999999999999999
88888888888888888888888888888880000000888888888888888888888888888888888999999999999999999999999999999999999999999999999999999999
88888888888888888888888888888800000008888888888888888888888888888888888888899999999999999999999999999999999999999999999999999999
88888888888888888888888888888800000008888888888888888888888888888888888888888889999999999999999999999999999999999999999999999999
88888888888888888888888888888800000000888888888888888888888888888888888888888888888899999999999999999999999999999999999999999999
88888888888888888888888888888800000000888888888888888888888888888888888888888888888888888899999999999999999999999999999999999999
88888888888888888888888888880000000008888888888888888888888888888888888888888888888888888888888899999999999999999999999999999999
88888888888888888888888888800000000088888888888888888888888888888888888888888888888888888888888888888888999999999999999999999999
88888888888888888888888888000000000088888888888888888888888888888888888888888888888888888888888888888888888888889999999999999999
88888888888888888888888888000000000000888888888888888888888888888888888888888888888888888888888888888888888888888888888888999999
88888888888888888888888888000000000000888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888888888888888800000000000000888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888888888888800000000000000000888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888888888888000000000000000000088888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888888888880000000000000000000000888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888888888000000000000000000000000000888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888800000000000000000000000000000000008888888888888888888888888888888888888888888888888888888888888888888888888888888888
88880000000000000000000000000000000000000000000000008888888888888888888888888888888888888888888888888888888888888888888888888888
00000000000000000000000000000000000000000000000000000000000000000888888888888888888888888888888888888888888888888888888888888888
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000088888888888888888888
__label__
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008eef00000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008eef00000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008eef00000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000eeeef00000000000000000000000000
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeeeef00000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeef00000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888880000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeffffffffffffffffffffffffeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeee22222222eeeeeeee222222222222eeeeeeeeeeeeeeeeeeeeeeee22220000000000000000000000000000000000000000000000000000000000000000
eeeeeeee22222222eeeeeeee22222222222eeeeeeeeeeeeeeeeeeeeeeee222220000000000000000000000000000000000000000000000000000000000000000
eeeeeeee22222222eeeeeeee2222222222eeeeeeeeeeeeeeeeeeeeeeee2222220000000000000000000000000000000000000000000000000000000000000000
eeeeeeee22222222eeeeeeee222222222eeeeeeeeeeeeeeeeeeeeeeee22222220000000000000000000000000000000000000000000000000000000000000000
eeeeeeee22222222eeeeeeee22222222eeeeeeeeeeeeeeeeeeeeeeee222222220000000000000000000000000000000000000000000000000000000000000000
eeeeeeee22222222eeeeeeee2222222eeeeeeeeeeeeeeeeeeeeeeee2222222220000000000000000000000000000000000000000000000000000000000000000
eeeeeeee2222222222222222222222eeeeeeeeeeeeeeeeeeeeeeee22222222220000000000000000000000000000000000000000000000000000000000000000
ddddddddeeeeeeee22222222eeeeeffffffffffffffffffffffffeee000000000000000000000000000000000000000000000000000000000000000000000000
dddddddd22222222eeeeeeee2222eeeeeeeeeeeeeeeeeeeeeeee2222000000000000000000000000000000000000000000000000000000000000000000000000
dddddddd22222222eeeeeeee222eeeeeeeeeeeeeeeeeeeeeeee22222000000000000000000000000000000000000000000000000000000000000000000000000
dddddddd22222222eeeeeeee22eeeeeeeeeeeeeeeeeeeeeeee222222000000000000000000000000000000000000000000000000000000000000000000000000
dddddddd22222222eeeeeeee2eeeeeeeeeeeeeeeeeeeeeeee2222222000000000000000000000000000000000000000000000000000000000000000000000000
dddddddd22222222eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee22222222000000000000000000000000000000000000000000000000000000000000000000000000
dddddddd11111111eeeeeeee22222222222222222222222211111111000000000000000000000000000000000000000000000000000000000000000000000000
ddddddddeeeeeeeeeeeeeeeeffffffffffffffffffffffffeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77000000077007700077770077777777777700000000000077700777770000770000000000000000000000000000000000000000000000000000000000000000
77770000700000070777777070700707777700000000000070000007777007770000000000000000000000000000000000000000000000000000000000000000
77777700000000007777777777777777700000000000000070000007077777700000000000000000000000000000000000000000000000000000000000000000
77777777000000007777777770000007707707777777077700000000007777000000000000000000000000000000000000000000000000000000000000000000
77777777000000007777777770000007707707777000070700000000007777000000000000000000000000000000000000000000000000000000000000000000
77777700700000077777777777777777700707070000777770000007077777700000000000000000000000000000000000000000000000000000000000000000
77770000077007700777777070700707777707770007707070000007777007770000000000000000000000000000000000000000000000000000000000000000
77000000000000000077770077777777777707777777777077700777770000770000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000888888888888888800990099009900990000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000800000000000000090000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000800000000000000890000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000888888888888888899009900990099000000000000000000000000000000000000000000
44444444400000000000000a00000000000000000000000000000000009900990099009900990099009900990000000000000000000000000000000000000000
4aaa44aa44444444400000a000000000000000000000000000000000000000000000000900000000000000090000000000000000000000000000000000000000
4aaa44aa4aaa4aaa4444444400000000000000000000000000000000900000000000000090000000000000000000000000000000000000000000000000000000
4aaa44aa4aaa4aaa4aaaaaa400000000000000000000000000000000900000000000000090000000000000000000000000000000000000000000000000000000
4aaaa4aa4aaa4aaa4aaaaaa400000000000000000000000000000000000000000000000900000000000000090000000000000000000000000000000000000000
4aaaa4aa4aaa4aaa4aaaaaa400000000000000000000000000000000000000000000000900000000000000090000000000000000000000000000000000000000
4aaaaaaa4aaa4aaa4aaaaaa400000000000000000000000000000000900000000000000090000000000000000000000000000000000000000000000000000000
4aaaaaaa4aaaaaaa4aaaaaa400000000000000000000000000000000900000000000000090000000000000000000000000000000000000000000000000000000
4aaaaaaa4aaaaa444aaa444400000000000000000000000000000000000000000000000900000000000000090000000000000000000000000000000000000000
4aaa4aaa4aaaaaa44aaaaaa400000000000000000000000000000000000000000088800900008880000000890000000000000000000000000000000000000000
4aaa44aa4aaa4aaa4444aaa400000000000000000000000000000000988800000088800090008880088800000000000000000000000000000000000000000000
4aaa44aa4aaa4aaa4aaaaaa400000000000000000000000000000000988800000088888090008880088800000007700000066600000000000000000000000000
444444444aaa4aaa4aaaaaa400000000000000000000000000000000088888000088888988000000088888090077770000066600000000000000000000000000
00000000444444444aaaaaa400000000000000000000000000000000088888000088888988000000088888090077770000066600000000000000000000000000
0000000000000000444444a400000000000000000000000000000000988888000000000098880000088888000000000000000000000000000000000000000000
00000000000000000000000a00000000000000000000000000000000990099009900990099889900990099000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111111
00000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000001
00000000000000000000000000000000000000000000000000000000000000000000000010006600660666000000000000066606060000000000000606066601
11111111111100000000000000000000000000000000000000000000000000000000000010060006000606006006060000000606060000060600000606060601
10000000000100000000000000000000000000000000000000000000000000000000000010066606000660000000600000066606660000066600000666066601
10000000000100000000000000000000000000000000000000000000000000000000000010000606000606006000600000060000060000000600000006060601
10000000000100000000000000000000000000000000000000000000000000000000000010066000660606000006060000066600060000066000000006066601
10000000000100000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000001
10000000000100000000000000000000000000000000000000000000000000000000000010066600660066000000000000066606660000000000000666060001
10000000000100000000000000000000000000000000000000000000000000000000000010060606060600006006060000060606060000060600000606060001
10000000000100000000000000000000000000000000000000000000000000000000000010066606060666000000600000066606060000066600000666066601
10000000000100000000000000000000000000000000000000000000000000000000000010060006060006006000600000060606060000000600000006060601
10000000000100000000000000000000000000000000000000000000000000000000000010060006600660000006060000066606660000066000000006066601
10000000000100000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000001
11111111111100000000000000000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10666006600660000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10600060006000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10660066606000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10600000606000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10666066000660000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10666066606060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10606060606060060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10660066606060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10606060606660060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10606060606660000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111

__sfx__
000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400002455024550245502455024550245502455000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400002455024550245502455024550245502455000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400002455024550245502455024550245502455000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400002455024550245502455024550245502455000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
