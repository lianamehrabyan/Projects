u0=imread('Image1.jpg'); 
imshow(u0,[])
figure()
%Optimal values for c in chosen cases
%image 1 c=0.0001,
%image 2 c=0.00002,
%image 2 c=0.00008,
u_new=PDE(u0, 0.0001);
imshow(u_new,[])