function [tfld,bfld]=topbot(g,fld);
%function [tfld,bfld] = mit_bottomfield(g,fld); or
%function [tfld,bfld] = mit_bottomfield(cmask,fld);

  if isstruct(g)
    cm = change(g.cmask,'==',NaN,0);
  else
    cm = change(g,'==',NaN,0);
  end
  tfld = repmat(NaN,size(fld(:,:,1,:)));
  bfld = repmat(NaN,size(fld(:,:,1,:)));
  for kx=1:size(fld,1);
    for ky=1:size(fld,2);
      kt=min(find(cm(kx,ky,:)>0));
      kb=max(find(cm(kx,ky,:)>0));
      if kt > 0;
	tfld(kx,ky,1,:) = fld(kx,ky,kt,:);
	bfld(kx,ky,1,:) = fld(kx,ky,kb,:);
      end
    end
  end
  tfld = squeeze(tfld);
  bfld = squeeze(bfld);
  
  return
  