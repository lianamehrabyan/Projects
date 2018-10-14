function u_r = PDE(image, c)
u0=image;

[n,m,k]=size(u0);

delta_t=0.03;
delta_s=1/(n-1);
p=c*delta_t/delta_s^2; 
tmax=2*delta_t;
t=0;
u_new=zeros(n,m);

for k=1:1:n %filling the boundaries for n 
  u_new(k,1)=u0(k,1);
  u_new(k,m)=u0(k,m);
end

for k=1:1:m %filling the boundaries for m
  u_new(1,k)=u0(1,k);  
  u_new(n,k)=u0(n,k);
end

while t<tmax
  for i = 2:1:n-1;
    
    for j = 2:1:m-1; 
      u_new(i, j) = u0(i,j)+ p*u0(i+1, j) + p*u0(i-1, j) - 4*p*u0(i, j) + p*u0(i, j+1) + p*u0(i, j-1);
    end
  end
  u0 = u_new;
  t=t+delta_t;
end
u_r=u_new;
end

