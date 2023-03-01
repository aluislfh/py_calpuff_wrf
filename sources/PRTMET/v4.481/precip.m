function comadreja
 clear all
  data1 =load('prec1.grd');
  data2 =load('prec8.grd');
  data3 =load('prec11.grd');
for j=1:264
    for i=1:200
        p1(i,j)=[data1(i,j)];
        p2(i,j)=[data2(i,j)];
        p3(i,j)=[data3(i,j)];
    end
end
for j=1:264
    for i=1:200
       pr(i,j)=p1(i,j)+p2(i,j)+p3(i,j);
     
    end
end

[C,h] = contour(pr);
clabel(C,h,'LabelSpacing',144);
colormap jet
