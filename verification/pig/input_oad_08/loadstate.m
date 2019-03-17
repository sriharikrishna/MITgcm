expdir = '../run';
iter=Inf;
g=mit_loadgrid;nx=g.nx; ny = g.ny;
grd = rdmnc(fullfile(expdir,'grid.*'),'Ro_surf','R_low','Depth');
g.depth=-grd.R_low;
g.ro_surf=-grd.Ro_surf;

DZ = repmat(reshape(g.dz,[1 1 g.nz]),[g.nx g.ny 1]);
s2d = rdmnc(fullfile(expdir,'surfDiag.*'),iter);
s3d = rdmnc(fullfile(expdir,'dynDiag.*'),iter);
e = s2d.ETAN(1:g.nx,1:g.ny,:,:);
t = s3d.THETA(1:g.nx,1:g.ny,:,:);
s = s3d.SALT(1:g.nx,1:g.ny,:,:);
uh = s3d.UVELMASS(1:g.nx,1:g.ny,:,:);
vh = s3d.VVELMASS(1:g.nx,1:g.ny,:,:);
% $$$ state = rdmnc(fullfile(expdir,'state.*'),iter);
% $$$ if isempty(state)
% $$$   [u,iter]=rdmds('U',iter);
% $$$   v=rdmds('V',iter);
% $$$   w=rdmds('W',iter);
% $$$   t=rdmds('T',iter);
% $$$   s=rdmds('S',iter);
% $$$   e=rdmds('Eta',iter);
% $$$   ph=rdmds('PH',iter);
% $$$ else
% $$$   u=state.U(1:g.nx,1:g.ny,:,:);
% $$$   v=state.V(1:g.nx,1:g.ny,:,:);
% $$$   w=state.W(1:g.nx,1:g.ny,:,:);
% $$$   t=state.Temp(1:g.nx,1:g.ny,:,:);
% $$$   s=state.S(1:g.nx,1:g.ny,:,:);
% $$$   e=state.Eta(1:g.nx,1:g.ny,:,:);
% $$$   phstate = rdmnc(fullfile(expdir,'phiHyd.*'),'phiHyd',iter);
% $$$   if size(e,3) > 1
% $$$     ph=phstate.phiHyd(1:g.nx,1:g.ny,:,[1 1:end]);
% $$$   else
% $$$     ph=phstate.phiHyd(1:g.nx,1:g.ny,:,:);
% $$$   end
% $$$ end
% $$$ ph0 = g.gravity*g.zc;
% $$$ msk = repmat(g.cmask,[1 1 1 size(u,4)]);
% $$$ %prs = g.rhonil*(ph+repmat(reshape(g.zc/g.gravity,[1 1 g.nz]),[nx ny 1 size(u,4)]));
% $$$ %prs = g.rhonil*(ph+repmat(reshape(e/g.gravity,[nx ny 1 size(u,4)]),[1 1 g.nz 1]));
% $$$ %prs = prs.*msk;
% $$$ hfw=change(g.hfacw,'==',NaN,0);
% $$$ hfs=change(g.hfacs,'==',NaN,0);
% $$$ uh = u;
% $$$ vh = v;
% $$$ for k=1:size(u,4)
% $$$   uh(:,:,:,k) = u(:,:,:,k).*hfw;
% $$$   vh(:,:,:,k) = v(:,:,:,k).*hfs;
% $$$ end
% $$$ [uc,vc,wc]=mit_velc(u,v,w);
[uhc,vhc]=mit_velc(uh,vh);

hfc=change(g.hfacc,'==',NaN,0);

xg=repmat(reshape(g.xg,[size(g.xg) 1]),[1 1 g.nz])/1000;
xc=repmat(reshape(g.xc,[size(g.xg) 1]),[1 1 g.nz])/1000;
yc=repmat(reshape(g.yc,[size(g.yg) 1]),[1 1 g.nz])/1000;
yg=repmat(reshape(g.yg,[size(g.yg) 1]),[1 1 g.nz])/1000;

fld=t;
%fld=uc;

% define section along main axis of PIG
[r,lo,la]=m_lldist([-102 -99.4],[-75 -75.3],20);
dd=m_lldist(lo,la);
xd = [0;cumsum(dd)];
for k=1:size(fld,3);
  fldsec(:,k) = interp2(g.lonc,g.latc,(fld(:,:,k))',lo,la,'nearest');
end
depsec = -interp2(g.lonc,g.latc,g.depth',lo,la,'nearest');
rossec = -interp2(g.lonc,g.latc,g.ro_surf',lo,la,'nearest');

% bottom
landcolor = [1 1 1]*.7;
icecolor = [1 1 1]*1;

nn=length(xd);
ix = 1:nn; i=nn; k=0; while i > 1; ix=ix([1:i i i+1:nn+k]); i=i-1; k=k+1; end
xb = xd(ix);
zb = depsec([1 ix(1:end-1)]);
zb(end) = zb(1);
% top
zt = rossec([1 ix(1:end-1)]);
zt(end) = zt(1);

cax = [min(sq(fldsec(:))) max(sq(fldsec(:)))];
flev = cax(1):diff(cax)/20:cax(2);
h=pcol(xd,-g.zg,sq(fldsec)'); shading flat
hold on
fhb=fill([xb(1) xb' xb(end)],[-1000 zb -1000], ...
	 landcolor,'edgecolor','none');
fht=fill(xb,zt,icecolor,'edgecolor','k');
% $$$ fhb = plot(xb(:),sq(zb),'k');
% $$$ fht = plot(xb(:),sq(zt),'k');
hold off
caxis(cax);colorbar

pause;

m_proj('lambert','lon',[min(g.xg(:)) max(g.xg(:))+.125], ...
       'lat',[min(g.yg(:)) max(g.yg(:))+.125*cos(max(g.yg(:))*pi/180)])
[xx,yy]=m_ll2xy(g.xg,g.yg);
[tfld,bfld] = topbot(g,fld);
clf
pcol(xx',yy',sq(tfld)');
m_grid;
hold on
sdz=sum(DZ.*hfc,3);
m_quiver(g.xc',g.yc',(sum(uhc.*DZ,3)./sdz)',(sum(vhc.*DZ,3)./sdz)',4,'k')
m_contour(g.lonc,g.latc,grd.Ro_surf',[-1 -1],'k')
hold off
%caxis([-2 -1.9])
colorbar

return
clf
pcol(xx',yy',sq(sq(grd.Ro_surf)-sq(grd.R_low))')
hold on
m_contour(g.xc',g.yc',(sq(grd.Ro_surf)-sq(grd.R_low))',[0:100:1000],'k')
caxis([0 700])
colorbar
m_grid

[x,y,z]=meshgrid(g.long,g.latg,g.zg);
sx = [-102:.2:-100];
sy = [-75.3:.1:-74.6];
sz = [-15 -500];
slice(x,y,-z,permute(sq(fld),[2 1 3]),sx,sy,sz);
shading flat 
camlight
%caxis([-2 -1.88])

[ut,ub] = topbot(g,uhc./g.hfacc);
[vt,vb] = topbot(g,vhc./g.hfacc);
sdz=sum(DZ.*hfc,3);
uu=(sum(uhc.*DZ,3)./sdz);
vv=(sum(vhc.*DZ,3)./sdz);

pcol(xx',yy',-sq(s2d.SHIfwFlx)'/1000*31104000);
m_grid;
hold on
m_quiver(g.xc',g.yc',uu',vv',2,'k')
m_contour(g.lonc,g.latc,grd.Ro_surf',[-1 -1],'k')
hold off
%caxis([-2 -1.9])
colorbar
