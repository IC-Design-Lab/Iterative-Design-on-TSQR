`ifdef ST_WIDTH_INF_4        
gol_yjp0   = gol_col[127:64];    
gol_yjp1   = gol_col[63 : 0];    
dut_yjp0   = dut_col[127:64];    
dut_yjp1   = dut_col[63 : 0];    

//if(ieee754_to_fp(gol_yjp0[63:32])<0) begin gol_re_yjp0  =-ieee754_to_fp(gol_yjp0[63:32]); end else begin gol_re_yjp0=ieee754_to_fp(gol_yjp0[63:32]); end     
//if(ieee754_to_fp(gol_yjp1[63:32])<0) begin gol_re_yjp1  =-ieee754_to_fp(gol_yjp1[63:32]); end else begin gol_re_yjp1=ieee754_to_fp(gol_yjp1[63:32]); end     
//if(ieee754_to_fp(dut_yjp0[63:32])<0) begin dut_re_yjp0  =-ieee754_to_fp(dut_yjp0[63:32]); end else begin dut_re_yjp0=ieee754_to_fp(dut_yjp0[63:32]); end     
//if(ieee754_to_fp(dut_yjp1[63:32])<0) begin dut_re_yjp1  =-ieee754_to_fp(dut_yjp1[63:32]); end else begin dut_re_yjp1=ieee754_to_fp(dut_yjp1[63:32]); end     
//if(ieee754_to_fp(gol_yjp0[31: 0])<0) begin gol_im_yjp0  =-ieee754_to_fp(gol_yjp0[31: 0]); end else begin gol_im_yjp0=ieee754_to_fp(gol_yjp0[31: 0]); end     
//if(ieee754_to_fp(gol_yjp1[31: 0])<0) begin gol_im_yjp1  =-ieee754_to_fp(gol_yjp1[31: 0]); end else begin gol_im_yjp1=ieee754_to_fp(gol_yjp1[31: 0]); end     
//if(ieee754_to_fp(dut_yjp0[31: 0])<0) begin dut_im_yjp0  =-ieee754_to_fp(dut_yjp0[31: 0]); end else begin dut_im_yjp0=ieee754_to_fp(dut_yjp0[31: 0]); end     
//if(ieee754_to_fp(dut_yjp1[31: 0])<0) begin dut_im_yjp1  =-ieee754_to_fp(dut_yjp1[31: 0]); end else begin dut_im_yjp1=ieee754_to_fp(dut_yjp1[31: 0]); end     

gol_re_yjp0=ieee754_to_fp(gol_yjp0[63:32]);      
gol_re_yjp1=ieee754_to_fp(gol_yjp1[63:32]);      
dut_re_yjp0=ieee754_to_fp(dut_yjp0[63:32]);      
dut_re_yjp1=ieee754_to_fp(dut_yjp1[63:32]);      
gol_im_yjp0=ieee754_to_fp(gol_yjp0[31: 0]);      
gol_im_yjp1=ieee754_to_fp(gol_yjp1[31: 0]);      
dut_im_yjp0=ieee754_to_fp(dut_yjp0[31: 0]);      
dut_im_yjp1=ieee754_to_fp(dut_yjp1[31: 0]);      

if(gol_yjp0[63:32]==0) begin err_re_per0   = (dut_re_yjp0-gol_re_yjp0); end else begin err_re_per0   = ((dut_re_yjp0  -gol_re_yjp0  )/gol_re_yjp0  )*100; end
if(gol_yjp1[63:32]==0) begin err_re_per1   = (dut_re_yjp1-gol_re_yjp1); end else begin err_re_per1   = ((dut_re_yjp1  -gol_re_yjp1  )/gol_re_yjp1  )*100; end
if(gol_yjp0[31:0 ]==0) begin err_im_per0   = (dut_im_yjp0-gol_im_yjp0); end else begin err_im_per0   = ((dut_im_yjp0  -gol_im_yjp0  )/gol_im_yjp0  )*100; end
if(gol_yjp1[31:0 ]==0) begin err_im_per1   = (dut_im_yjp1-gol_im_yjp1); end else begin err_im_per1   = ((dut_im_yjp1  -gol_im_yjp1  )/gol_im_yjp1  )*100; end

if(err_re_per0<0) err_re_per0   = -err_re_per0  ; else err_re_per0   = err_re_per0  ;
if(err_re_per1<0) err_re_per1   = -err_re_per1  ; else err_re_per1   = err_re_per1  ;
if(err_im_per0<0) err_im_per0   = -err_im_per0  ; else err_im_per0   = err_im_per0  ;
if(err_im_per1<0) err_im_per1   = -err_im_per1  ; else err_im_per1   = err_im_per1  ;
`endif // ST_WIDTH_INF_16      

`ifdef ST_WIDTH_INF_8        
gol_yjp0   = gol_col[255:192];    
gol_yjp1   = gol_col[191 : 128];    
dut_yjp0   = dut_col[255:192];    
dut_yjp1   = dut_col[191 : 128];

gol_yjp2   = gol_col[127:64];    
gol_yjp3   = gol_col[63 : 0];    
dut_yjp2   = dut_col[127:64];    
dut_yjp3   = dut_col[63 : 0]; 

//if(ieee754_to_fp(gol_yjp0[63:32])<0) begin gol_re_yjp0  =-ieee754_to_fp(gol_yjp0[63:32]); end else begin gol_re_yjp0=ieee754_to_fp(gol_yjp0[63:32]); end     
//if(ieee754_to_fp(gol_yjp1[63:32])<0) begin gol_re_yjp1  =-ieee754_to_fp(gol_yjp1[63:32]); end else begin gol_re_yjp1=ieee754_to_fp(gol_yjp1[63:32]); end     
//if(ieee754_to_fp(dut_yjp0[63:32])<0) begin dut_re_yjp0  =-ieee754_to_fp(dut_yjp0[63:32]); end else begin dut_re_yjp0=ieee754_to_fp(dut_yjp0[63:32]); end     
//if(ieee754_to_fp(dut_yjp1[63:32])<0) begin dut_re_yjp1  =-ieee754_to_fp(dut_yjp1[63:32]); end else begin dut_re_yjp1=ieee754_to_fp(dut_yjp1[63:32]); end     
//if(ieee754_to_fp(gol_yjp0[31: 0])<0) begin gol_im_yjp0  =-ieee754_to_fp(gol_yjp0[31: 0]); end else begin gol_im_yjp0=ieee754_to_fp(gol_yjp0[31: 0]); end     
//if(ieee754_to_fp(gol_yjp1[31: 0])<0) begin gol_im_yjp1  =-ieee754_to_fp(gol_yjp1[31: 0]); end else begin gol_im_yjp1=ieee754_to_fp(gol_yjp1[31: 0]); end     
//if(ieee754_to_fp(dut_yjp0[31: 0])<0) begin dut_im_yjp0  =-ieee754_to_fp(dut_yjp0[31: 0]); end else begin dut_im_yjp0=ieee754_to_fp(dut_yjp0[31: 0]); end     
//if(ieee754_to_fp(dut_yjp1[31: 0])<0) begin dut_im_yjp1  =-ieee754_to_fp(dut_yjp1[31: 0]); end else begin dut_im_yjp1=ieee754_to_fp(dut_yjp1[31: 0]); end     

gol_re_yjp0=ieee754_to_fp(gol_yjp0[63:32]);      
gol_re_yjp1=ieee754_to_fp(gol_yjp1[63:32]);      
dut_re_yjp0=ieee754_to_fp(dut_yjp0[63:32]);      
dut_re_yjp1=ieee754_to_fp(dut_yjp1[63:32]);      
gol_im_yjp0=ieee754_to_fp(gol_yjp0[31: 0]);      
gol_im_yjp1=ieee754_to_fp(gol_yjp1[31: 0]);      
dut_im_yjp0=ieee754_to_fp(dut_yjp0[31: 0]);      
dut_im_yjp1=ieee754_to_fp(dut_yjp1[31: 0]);     

gol_re_yjp2=ieee754_to_fp(gol_yjp2[63:32]);      
gol_re_yjp3=ieee754_to_fp(gol_yjp3[63:32]);      
dut_re_yjp2=ieee754_to_fp(dut_yjp2[63:32]);      
dut_re_yjp3=ieee754_to_fp(dut_yjp3[63:32]);      
gol_im_yjp2=ieee754_to_fp(gol_yjp2[31: 0]);      
gol_im_yjp3=ieee754_to_fp(gol_yjp3[31: 0]);      
dut_im_yjp2=ieee754_to_fp(dut_yjp2[31: 0]);      
dut_im_yjp3=ieee754_to_fp(dut_yjp3[31: 0]);      


if(gol_yjp0[63:32]==0) begin err_re_per0   = (dut_re_yjp0-gol_re_yjp0); end else begin err_re_per0   = ((dut_re_yjp0  -gol_re_yjp0  )/gol_re_yjp0  )*100; end
if(gol_yjp1[63:32]==0) begin err_re_per1   = (dut_re_yjp1-gol_re_yjp1); end else begin err_re_per1   = ((dut_re_yjp1  -gol_re_yjp1  )/gol_re_yjp1  )*100; end
if(gol_yjp0[31:0 ]==0) begin err_im_per0   = (dut_im_yjp0-gol_im_yjp0); end else begin err_im_per0   = ((dut_im_yjp0  -gol_im_yjp0  )/gol_im_yjp0  )*100; end
if(gol_yjp1[31:0 ]==0) begin err_im_per1   = (dut_im_yjp1-gol_im_yjp1); end else begin err_im_per1   = ((dut_im_yjp1  -gol_im_yjp1  )/gol_im_yjp1  )*100; end

if(gol_yjp2[63:32]==0) begin err_re_per2   = (dut_re_yjp2-gol_re_yjp2); end else begin err_re_per2   = ((dut_re_yjp2  -gol_re_yjp2  )/gol_re_yjp2  )*100; end
if(gol_yjp3[63:32]==0) begin err_re_per3   = (dut_re_yjp3-gol_re_yjp3); end else begin err_re_per3   = ((dut_re_yjp3  -gol_re_yjp3  )/gol_re_yjp3  )*100; end
if(gol_yjp2[31:0 ]==0) begin err_im_per2   = (dut_im_yjp2-gol_im_yjp2); end else begin err_im_per2   = ((dut_im_yjp2  -gol_im_yjp2  )/gol_im_yjp2  )*100; end
if(gol_yjp3[31:0 ]==0) begin err_im_per3   = (dut_im_yjp3-gol_im_yjp3); end else begin err_im_per3   = ((dut_im_yjp3  -gol_im_yjp3  )/gol_im_yjp3  )*100; end


if(err_re_per0<0) err_re_per0   = -err_re_per0  ; else err_re_per0   = err_re_per0  ;
if(err_re_per1<0) err_re_per1   = -err_re_per1  ; else err_re_per1   = err_re_per1  ;
if(err_im_per0<0) err_im_per0   = -err_im_per0  ; else err_im_per0   = err_im_per0  ;
if(err_im_per1<0) err_im_per1   = -err_im_per1  ; else err_im_per1   = err_im_per1  ;

if(err_re_per2<0) err_re_per2   = -err_re_per2  ; else err_re_per2   = err_re_per2  ;
if(err_re_per3<0) err_re_per3   = -err_re_per3  ; else err_re_per3   = err_re_per3  ;
if(err_im_per2<0) err_im_per2   = -err_im_per2  ; else err_im_per2   = err_im_per2  ;
if(err_im_per3<0) err_im_per3   = -err_im_per3  ; else err_im_per3   = err_im_per3  ;

`endif // ST_WIDTH_INF_16 


`ifdef ST_WIDTH_INF_16

gol_yjp0 = gol_col[511:448];
dut_yjp0 = dut_col[511:448];

gol_yjp1 = gol_col[447:384];
dut_yjp1 = dut_col[447:384];

gol_yjp2 = gol_col[383:320];
dut_yjp2 = dut_col[383:320];

gol_yjp3 = gol_col[319:256];
dut_yjp3 = dut_col[319:256];

gol_yjp4 = gol_col[255:192];
dut_yjp4 = dut_col[255:192];

gol_yjp5 = gol_col[191:128];
dut_yjp5 = dut_col[191:128];

gol_yjp6 = gol_col[127:64];
dut_yjp6 = dut_col[127:64];

gol_yjp7 = gol_col[63:0];
dut_yjp7 = dut_col[63:0];

gol_re_yjp0 = ieee754_to_fp(gol_yjp0[63:32]);
dut_re_yjp0 = ieee754_to_fp(dut_yjp0[63:32]);
gol_im_yjp0 = ieee754_to_fp(gol_yjp0[31:0]);
dut_im_yjp0 = ieee754_to_fp(dut_yjp0[31:0]);
gol_re_yjp1 = ieee754_to_fp(gol_yjp1[63:32]);
dut_re_yjp1 = ieee754_to_fp(dut_yjp1[63:32]);
gol_im_yjp1 = ieee754_to_fp(gol_yjp1[31:0]);
dut_im_yjp1 = ieee754_to_fp(dut_yjp1[31:0]);
gol_re_yjp2 = ieee754_to_fp(gol_yjp2[63:32]);
dut_re_yjp2 = ieee754_to_fp(dut_yjp2[63:32]);
gol_im_yjp2 = ieee754_to_fp(gol_yjp2[31:0]);
dut_im_yjp2 = ieee754_to_fp(dut_yjp2[31:0]);
gol_re_yjp3 = ieee754_to_fp(gol_yjp3[63:32]);
dut_re_yjp3 = ieee754_to_fp(dut_yjp3[63:32]);
gol_im_yjp3 = ieee754_to_fp(gol_yjp3[31:0]);
dut_im_yjp3 = ieee754_to_fp(dut_yjp3[31:0]);
gol_re_yjp4 = ieee754_to_fp(gol_yjp4[63:32]);
dut_re_yjp4 = ieee754_to_fp(dut_yjp4[63:32]);
gol_im_yjp4 = ieee754_to_fp(gol_yjp4[31:0]);
dut_im_yjp4 = ieee754_to_fp(dut_yjp4[31:0]);
gol_re_yjp5 = ieee754_to_fp(gol_yjp5[63:32]);
dut_re_yjp5 = ieee754_to_fp(dut_yjp5[63:32]);
gol_im_yjp5 = ieee754_to_fp(gol_yjp5[31:0]);
dut_im_yjp5 = ieee754_to_fp(dut_yjp5[31:0]);
gol_re_yjp6 = ieee754_to_fp(gol_yjp6[63:32]);
dut_re_yjp6 = ieee754_to_fp(dut_yjp6[63:32]);
gol_im_yjp6 = ieee754_to_fp(gol_yjp6[31:0]);
dut_im_yjp6 = ieee754_to_fp(dut_yjp6[31:0]);
gol_re_yjp7 = ieee754_to_fp(gol_yjp7[63:32]);
dut_re_yjp7 = ieee754_to_fp(dut_yjp7[63:32]);
gol_im_yjp7 = ieee754_to_fp(gol_yjp7[31:0]);
dut_im_yjp7 = ieee754_to_fp(dut_yjp7[31:0]);

if(gol_yjp0[63:32] == 0) begin err_re_per0 = (dut_re_yjp0 - gol_re_yjp0); end else begin err_re_per0 = ((dut_re_yjp0 - gol_re_yjp0) / gol_re_yjp0) * 100; end
if(gol_yjp0[31:0] == 0) begin err_im_per0 = (dut_im_yjp0 - gol_im_yjp0); end else begin err_im_per0 = ((dut_im_yjp0 - gol_im_yjp0) / gol_im_yjp0) * 100; end
if(gol_yjp1[63:32] == 0) begin err_re_per1 = (dut_re_yjp1 - gol_re_yjp1); end else begin err_re_per1 = ((dut_re_yjp1 - gol_re_yjp1) / gol_re_yjp1) * 100; end
if(gol_yjp1[31:0] == 0) begin err_im_per1 = (dut_im_yjp1 - gol_im_yjp1); end else begin err_im_per1 = ((dut_im_yjp1 - gol_im_yjp1) / gol_im_yjp1) * 100; end
if(gol_yjp2[63:32] == 0) begin err_re_per2 = (dut_re_yjp2 - gol_re_yjp2); end else begin err_re_per2 = ((dut_re_yjp2 - gol_re_yjp2) / gol_re_yjp2) * 100; end
if(gol_yjp2[31:0] == 0) begin err_im_per2 = (dut_im_yjp2 - gol_im_yjp2); end else begin err_im_per2 = ((dut_im_yjp2 - gol_im_yjp2) / gol_im_yjp2) * 100; end
if(gol_yjp3[63:32] == 0) begin err_re_per3 = (dut_re_yjp3 - gol_re_yjp3); end else begin err_re_per3 = ((dut_re_yjp3 - gol_re_yjp3) / gol_re_yjp3) * 100; end
if(gol_yjp3[31:0] == 0) begin err_im_per3 = (dut_im_yjp3 - gol_im_yjp3); end else begin err_im_per3 = ((dut_im_yjp3 - gol_im_yjp3) / gol_im_yjp3) * 100; end
if(gol_yjp4[63:32] == 0) begin err_re_per4 = (dut_re_yjp4 - gol_re_yjp4); end else begin err_re_per4 = ((dut_re_yjp4 - gol_re_yjp4) / gol_re_yjp4) * 100; end
if(gol_yjp4[31:0] == 0) begin err_im_per4 = (dut_im_yjp4 - gol_im_yjp4); end else begin err_im_per4 = ((dut_im_yjp4 - gol_im_yjp4) / gol_im_yjp4) * 100; end
if(gol_yjp5[63:32] == 0) begin err_re_per5 = (dut_re_yjp5 - gol_re_yjp5); end else begin err_re_per5 = ((dut_re_yjp5 - gol_re_yjp5) / gol_re_yjp5) * 100; end
if(gol_yjp5[31:0] == 0) begin err_im_per5 = (dut_im_yjp5 - gol_im_yjp5); end else begin err_im_per5 = ((dut_im_yjp5 - gol_im_yjp5) / gol_im_yjp5) * 100; end
if(gol_yjp6[63:32] == 0) begin err_re_per6 = (dut_re_yjp6 - gol_re_yjp6); end else begin err_re_per6 = ((dut_re_yjp6 - gol_re_yjp6) / gol_re_yjp6) * 100; end
if(gol_yjp6[31:0] == 0) begin err_im_per6 = (dut_im_yjp6 - gol_im_yjp6); end else begin err_im_per6 = ((dut_im_yjp6 - gol_im_yjp6) / gol_im_yjp6) * 100; end
if(gol_yjp7[63:32] == 0) begin err_re_per7 = (dut_re_yjp7 - gol_re_yjp7); end else begin err_re_per7 = ((dut_re_yjp7 - gol_re_yjp7) / gol_re_yjp7) * 100; end
if(gol_yjp7[31:0] == 0) begin err_im_per7 = (dut_im_yjp7 - gol_im_yjp7); end else begin err_im_per7 = ((dut_im_yjp7 - gol_im_yjp7) / gol_im_yjp7) * 100; end

if(err_re_per0 < 0) err_re_per0 = -err_re_per0; else err_re_per0 = err_re_per0;
if(err_im_per0 < 0) err_im_per0 = -err_im_per0; else err_im_per0 = err_im_per0;
if(err_re_per1 < 0) err_re_per1 = -err_re_per1; else err_re_per1 = err_re_per1;
if(err_im_per1 < 0) err_im_per1 = -err_im_per1; else err_im_per1 = err_im_per1;
if(err_re_per2 < 0) err_re_per2 = -err_re_per2; else err_re_per2 = err_re_per2;
if(err_im_per2 < 0) err_im_per2 = -err_im_per2; else err_im_per2 = err_im_per2;
if(err_re_per3 < 0) err_re_per3 = -err_re_per3; else err_re_per3 = err_re_per3;
if(err_im_per3 < 0) err_im_per3 = -err_im_per3; else err_im_per3 = err_im_per3;
if(err_re_per4 < 0) err_re_per4 = -err_re_per4; else err_re_per4 = err_re_per4;
if(err_im_per4 < 0) err_im_per4 = -err_im_per4; else err_im_per4 = err_im_per4;
if(err_re_per5 < 0) err_re_per5 = -err_re_per5; else err_re_per5 = err_re_per5;
if(err_im_per5 < 0) err_im_per5 = -err_im_per5; else err_im_per5 = err_im_per5;
if(err_re_per6 < 0) err_re_per6 = -err_re_per6; else err_re_per6 = err_re_per6;
if(err_im_per6 < 0) err_im_per6 = -err_im_per6; else err_im_per6 = err_im_per6;
if(err_re_per7 < 0) err_re_per7 = -err_re_per7; else err_re_per7 = err_re_per7;
if(err_im_per7 < 0) err_im_per7 = -err_im_per7; else err_im_per7 = err_im_per7;

`endif 

`ifdef ST_WIDTH_INF_512
gol_yjp0 = gol_col[16383:16320];
dut_yjp0 = dut_col[16383:16320];

gol_yjp1 = gol_col[16319:16256];
dut_yjp1 = dut_col[16319:16256];

gol_yjp2 = gol_col[16255:16192];
dut_yjp2 = dut_col[16255:16192];

gol_yjp3 = gol_col[16191:16128];
dut_yjp3 = dut_col[16191:16128];

gol_yjp4 = gol_col[16127:16064];
dut_yjp4 = dut_col[16127:16064];

gol_yjp5 = gol_col[16063:16000];
dut_yjp5 = dut_col[16063:16000];

gol_yjp6 = gol_col[15999:15936];
dut_yjp6 = dut_col[15999:15936];

gol_yjp7 = gol_col[15935:15872];
dut_yjp7 = dut_col[15935:15872];

gol_yjp8 = gol_col[15871:15808];
dut_yjp8 = dut_col[15871:15808];

gol_yjp9 = gol_col[15807:15744];
dut_yjp9 = dut_col[15807:15744];

gol_yjp10 = gol_col[15743:15680];
dut_yjp10 = dut_col[15743:15680];

gol_yjp11 = gol_col[15679:15616];
dut_yjp11 = dut_col[15679:15616];

gol_yjp12 = gol_col[15615:15552];
dut_yjp12 = dut_col[15615:15552];

gol_yjp13 = gol_col[15551:15488];
dut_yjp13 = dut_col[15551:15488];

gol_yjp14 = gol_col[15487:15424];
dut_yjp14 = dut_col[15487:15424];

gol_yjp15 = gol_col[15423:15360];
dut_yjp15 = dut_col[15423:15360];

gol_yjp16 = gol_col[15359:15296];
dut_yjp16 = dut_col[15359:15296];

gol_yjp17 = gol_col[15295:15232];
dut_yjp17 = dut_col[15295:15232];

gol_yjp18 = gol_col[15231:15168];
dut_yjp18 = dut_col[15231:15168];

gol_yjp19 = gol_col[15167:15104];
dut_yjp19 = dut_col[15167:15104];

gol_yjp20 = gol_col[15103:15040];
dut_yjp20 = dut_col[15103:15040];

gol_yjp21 = gol_col[15039:14976];
dut_yjp21 = dut_col[15039:14976];

gol_yjp22 = gol_col[14975:14912];
dut_yjp22 = dut_col[14975:14912];

gol_yjp23 = gol_col[14911:14848];
dut_yjp23 = dut_col[14911:14848];

gol_yjp24 = gol_col[14847:14784];
dut_yjp24 = dut_col[14847:14784];

gol_yjp25 = gol_col[14783:14720];
dut_yjp25 = dut_col[14783:14720];

gol_yjp26 = gol_col[14719:14656];
dut_yjp26 = dut_col[14719:14656];

gol_yjp27 = gol_col[14655:14592];
dut_yjp27 = dut_col[14655:14592];

gol_yjp28 = gol_col[14591:14528];
dut_yjp28 = dut_col[14591:14528];

gol_yjp29 = gol_col[14527:14464];
dut_yjp29 = dut_col[14527:14464];

gol_yjp30 = gol_col[14463:14400];
dut_yjp30 = dut_col[14463:14400];

gol_yjp31 = gol_col[14399:14336];
dut_yjp31 = dut_col[14399:14336];

gol_yjp32 = gol_col[14335:14272];
dut_yjp32 = dut_col[14335:14272];

gol_yjp33 = gol_col[14271:14208];
dut_yjp33 = dut_col[14271:14208];

gol_yjp34 = gol_col[14207:14144];
dut_yjp34 = dut_col[14207:14144];

gol_yjp35 = gol_col[14143:14080];
dut_yjp35 = dut_col[14143:14080];

gol_yjp36 = gol_col[14079:14016];
dut_yjp36 = dut_col[14079:14016];

gol_yjp37 = gol_col[14015:13952];
dut_yjp37 = dut_col[14015:13952];

gol_yjp38 = gol_col[13951:13888];
dut_yjp38 = dut_col[13951:13888];

gol_yjp39 = gol_col[13887:13824];
dut_yjp39 = dut_col[13887:13824];

gol_yjp40 = gol_col[13823:13760];
dut_yjp40 = dut_col[13823:13760];

gol_yjp41 = gol_col[13759:13696];
dut_yjp41 = dut_col[13759:13696];

gol_yjp42 = gol_col[13695:13632];
dut_yjp42 = dut_col[13695:13632];

gol_yjp43 = gol_col[13631:13568];
dut_yjp43 = dut_col[13631:13568];

gol_yjp44 = gol_col[13567:13504];
dut_yjp44 = dut_col[13567:13504];

gol_yjp45 = gol_col[13503:13440];
dut_yjp45 = dut_col[13503:13440];

gol_yjp46 = gol_col[13439:13376];
dut_yjp46 = dut_col[13439:13376];

gol_yjp47 = gol_col[13375:13312];
dut_yjp47 = dut_col[13375:13312];

gol_yjp48 = gol_col[13311:13248];
dut_yjp48 = dut_col[13311:13248];

gol_yjp49 = gol_col[13247:13184];
dut_yjp49 = dut_col[13247:13184];

gol_yjp50 = gol_col[13183:13120];
dut_yjp50 = dut_col[13183:13120];

gol_yjp51 = gol_col[13119:13056];
dut_yjp51 = dut_col[13119:13056];

gol_yjp52 = gol_col[13055:12992];
dut_yjp52 = dut_col[13055:12992];

gol_yjp53 = gol_col[12991:12928];
dut_yjp53 = dut_col[12991:12928];

gol_yjp54 = gol_col[12927:12864];
dut_yjp54 = dut_col[12927:12864];

gol_yjp55 = gol_col[12863:12800];
dut_yjp55 = dut_col[12863:12800];

gol_yjp56 = gol_col[12799:12736];
dut_yjp56 = dut_col[12799:12736];

gol_yjp57 = gol_col[12735:12672];
dut_yjp57 = dut_col[12735:12672];

gol_yjp58 = gol_col[12671:12608];
dut_yjp58 = dut_col[12671:12608];

gol_yjp59 = gol_col[12607:12544];
dut_yjp59 = dut_col[12607:12544];

gol_yjp60 = gol_col[12543:12480];
dut_yjp60 = dut_col[12543:12480];

gol_yjp61 = gol_col[12479:12416];
dut_yjp61 = dut_col[12479:12416];

gol_yjp62 = gol_col[12415:12352];
dut_yjp62 = dut_col[12415:12352];

gol_yjp63 = gol_col[12351:12288];
dut_yjp63 = dut_col[12351:12288];

gol_yjp64 = gol_col[12287:12224];
dut_yjp64 = dut_col[12287:12224];

gol_yjp65 = gol_col[12223:12160];
dut_yjp65 = dut_col[12223:12160];

gol_yjp66 = gol_col[12159:12096];
dut_yjp66 = dut_col[12159:12096];

gol_yjp67 = gol_col[12095:12032];
dut_yjp67 = dut_col[12095:12032];

gol_yjp68 = gol_col[12031:11968];
dut_yjp68 = dut_col[12031:11968];

gol_yjp69 = gol_col[11967:11904];
dut_yjp69 = dut_col[11967:11904];

gol_yjp70 = gol_col[11903:11840];
dut_yjp70 = dut_col[11903:11840];

gol_yjp71 = gol_col[11839:11776];
dut_yjp71 = dut_col[11839:11776];

gol_yjp72 = gol_col[11775:11712];
dut_yjp72 = dut_col[11775:11712];

gol_yjp73 = gol_col[11711:11648];
dut_yjp73 = dut_col[11711:11648];

gol_yjp74 = gol_col[11647:11584];
dut_yjp74 = dut_col[11647:11584];

gol_yjp75 = gol_col[11583:11520];
dut_yjp75 = dut_col[11583:11520];

gol_yjp76 = gol_col[11519:11456];
dut_yjp76 = dut_col[11519:11456];

gol_yjp77 = gol_col[11455:11392];
dut_yjp77 = dut_col[11455:11392];

gol_yjp78 = gol_col[11391:11328];
dut_yjp78 = dut_col[11391:11328];

gol_yjp79 = gol_col[11327:11264];
dut_yjp79 = dut_col[11327:11264];

gol_yjp80 = gol_col[11263:11200];
dut_yjp80 = dut_col[11263:11200];

gol_yjp81 = gol_col[11199:11136];
dut_yjp81 = dut_col[11199:11136];

gol_yjp82 = gol_col[11135:11072];
dut_yjp82 = dut_col[11135:11072];

gol_yjp83 = gol_col[11071:11008];
dut_yjp83 = dut_col[11071:11008];

gol_yjp84 = gol_col[11007:10944];
dut_yjp84 = dut_col[11007:10944];

gol_yjp85 = gol_col[10943:10880];
dut_yjp85 = dut_col[10943:10880];

gol_yjp86 = gol_col[10879:10816];
dut_yjp86 = dut_col[10879:10816];

gol_yjp87 = gol_col[10815:10752];
dut_yjp87 = dut_col[10815:10752];

gol_yjp88 = gol_col[10751:10688];
dut_yjp88 = dut_col[10751:10688];

gol_yjp89 = gol_col[10687:10624];
dut_yjp89 = dut_col[10687:10624];

gol_yjp90 = gol_col[10623:10560];
dut_yjp90 = dut_col[10623:10560];

gol_yjp91 = gol_col[10559:10496];
dut_yjp91 = dut_col[10559:10496];

gol_yjp92 = gol_col[10495:10432];
dut_yjp92 = dut_col[10495:10432];

gol_yjp93 = gol_col[10431:10368];
dut_yjp93 = dut_col[10431:10368];

gol_yjp94 = gol_col[10367:10304];
dut_yjp94 = dut_col[10367:10304];

gol_yjp95 = gol_col[10303:10240];
dut_yjp95 = dut_col[10303:10240];

gol_yjp96 = gol_col[10239:10176];
dut_yjp96 = dut_col[10239:10176];

gol_yjp97 = gol_col[10175:10112];
dut_yjp97 = dut_col[10175:10112];

gol_yjp98 = gol_col[10111:10048];
dut_yjp98 = dut_col[10111:10048];

gol_yjp99 = gol_col[10047:9984];
dut_yjp99 = dut_col[10047:9984];

gol_yjp100 = gol_col[9983:9920];
dut_yjp100 = dut_col[9983:9920];

gol_yjp101 = gol_col[9919:9856];
dut_yjp101 = dut_col[9919:9856];

gol_yjp102 = gol_col[9855:9792];
dut_yjp102 = dut_col[9855:9792];

gol_yjp103 = gol_col[9791:9728];
dut_yjp103 = dut_col[9791:9728];

gol_yjp104 = gol_col[9727:9664];
dut_yjp104 = dut_col[9727:9664];

gol_yjp105 = gol_col[9663:9600];
dut_yjp105 = dut_col[9663:9600];

gol_yjp106 = gol_col[9599:9536];
dut_yjp106 = dut_col[9599:9536];

gol_yjp107 = gol_col[9535:9472];
dut_yjp107 = dut_col[9535:9472];

gol_yjp108 = gol_col[9471:9408];
dut_yjp108 = dut_col[9471:9408];

gol_yjp109 = gol_col[9407:9344];
dut_yjp109 = dut_col[9407:9344];

gol_yjp110 = gol_col[9343:9280];
dut_yjp110 = dut_col[9343:9280];

gol_yjp111 = gol_col[9279:9216];
dut_yjp111 = dut_col[9279:9216];

gol_yjp112 = gol_col[9215:9152];
dut_yjp112 = dut_col[9215:9152];

gol_yjp113 = gol_col[9151:9088];
dut_yjp113 = dut_col[9151:9088];

gol_yjp114 = gol_col[9087:9024];
dut_yjp114 = dut_col[9087:9024];

gol_yjp115 = gol_col[9023:8960];
dut_yjp115 = dut_col[9023:8960];

gol_yjp116 = gol_col[8959:8896];
dut_yjp116 = dut_col[8959:8896];

gol_yjp117 = gol_col[8895:8832];
dut_yjp117 = dut_col[8895:8832];

gol_yjp118 = gol_col[8831:8768];
dut_yjp118 = dut_col[8831:8768];

gol_yjp119 = gol_col[8767:8704];
dut_yjp119 = dut_col[8767:8704];

gol_yjp120 = gol_col[8703:8640];
dut_yjp120 = dut_col[8703:8640];

gol_yjp121 = gol_col[8639:8576];
dut_yjp121 = dut_col[8639:8576];

gol_yjp122 = gol_col[8575:8512];
dut_yjp122 = dut_col[8575:8512];

gol_yjp123 = gol_col[8511:8448];
dut_yjp123 = dut_col[8511:8448];

gol_yjp124 = gol_col[8447:8384];
dut_yjp124 = dut_col[8447:8384];

gol_yjp125 = gol_col[8383:8320];
dut_yjp125 = dut_col[8383:8320];

gol_yjp126 = gol_col[8319:8256];
dut_yjp126 = dut_col[8319:8256];

gol_yjp127 = gol_col[8255:8192];
dut_yjp127 = dut_col[8255:8192];

gol_yjp128 = gol_col[8191:8128];
dut_yjp128 = dut_col[8191:8128];

gol_yjp129 = gol_col[8127:8064];
dut_yjp129 = dut_col[8127:8064];

gol_yjp130 = gol_col[8063:8000];
dut_yjp130 = dut_col[8063:8000];

gol_yjp131 = gol_col[7999:7936];
dut_yjp131 = dut_col[7999:7936];

gol_yjp132 = gol_col[7935:7872];
dut_yjp132 = dut_col[7935:7872];

gol_yjp133 = gol_col[7871:7808];
dut_yjp133 = dut_col[7871:7808];

gol_yjp134 = gol_col[7807:7744];
dut_yjp134 = dut_col[7807:7744];

gol_yjp135 = gol_col[7743:7680];
dut_yjp135 = dut_col[7743:7680];

gol_yjp136 = gol_col[7679:7616];
dut_yjp136 = dut_col[7679:7616];

gol_yjp137 = gol_col[7615:7552];
dut_yjp137 = dut_col[7615:7552];

gol_yjp138 = gol_col[7551:7488];
dut_yjp138 = dut_col[7551:7488];

gol_yjp139 = gol_col[7487:7424];
dut_yjp139 = dut_col[7487:7424];

gol_yjp140 = gol_col[7423:7360];
dut_yjp140 = dut_col[7423:7360];

gol_yjp141 = gol_col[7359:7296];
dut_yjp141 = dut_col[7359:7296];

gol_yjp142 = gol_col[7295:7232];
dut_yjp142 = dut_col[7295:7232];

gol_yjp143 = gol_col[7231:7168];
dut_yjp143 = dut_col[7231:7168];

gol_yjp144 = gol_col[7167:7104];
dut_yjp144 = dut_col[7167:7104];

gol_yjp145 = gol_col[7103:7040];
dut_yjp145 = dut_col[7103:7040];

gol_yjp146 = gol_col[7039:6976];
dut_yjp146 = dut_col[7039:6976];

gol_yjp147 = gol_col[6975:6912];
dut_yjp147 = dut_col[6975:6912];

gol_yjp148 = gol_col[6911:6848];
dut_yjp148 = dut_col[6911:6848];

gol_yjp149 = gol_col[6847:6784];
dut_yjp149 = dut_col[6847:6784];

gol_yjp150 = gol_col[6783:6720];
dut_yjp150 = dut_col[6783:6720];

gol_yjp151 = gol_col[6719:6656];
dut_yjp151 = dut_col[6719:6656];

gol_yjp152 = gol_col[6655:6592];
dut_yjp152 = dut_col[6655:6592];

gol_yjp153 = gol_col[6591:6528];
dut_yjp153 = dut_col[6591:6528];

gol_yjp154 = gol_col[6527:6464];
dut_yjp154 = dut_col[6527:6464];

gol_yjp155 = gol_col[6463:6400];
dut_yjp155 = dut_col[6463:6400];

gol_yjp156 = gol_col[6399:6336];
dut_yjp156 = dut_col[6399:6336];

gol_yjp157 = gol_col[6335:6272];
dut_yjp157 = dut_col[6335:6272];

gol_yjp158 = gol_col[6271:6208];
dut_yjp158 = dut_col[6271:6208];

gol_yjp159 = gol_col[6207:6144];
dut_yjp159 = dut_col[6207:6144];

gol_yjp160 = gol_col[6143:6080];
dut_yjp160 = dut_col[6143:6080];

gol_yjp161 = gol_col[6079:6016];
dut_yjp161 = dut_col[6079:6016];

gol_yjp162 = gol_col[6015:5952];
dut_yjp162 = dut_col[6015:5952];

gol_yjp163 = gol_col[5951:5888];
dut_yjp163 = dut_col[5951:5888];

gol_yjp164 = gol_col[5887:5824];
dut_yjp164 = dut_col[5887:5824];

gol_yjp165 = gol_col[5823:5760];
dut_yjp165 = dut_col[5823:5760];

gol_yjp166 = gol_col[5759:5696];
dut_yjp166 = dut_col[5759:5696];

gol_yjp167 = gol_col[5695:5632];
dut_yjp167 = dut_col[5695:5632];

gol_yjp168 = gol_col[5631:5568];
dut_yjp168 = dut_col[5631:5568];

gol_yjp169 = gol_col[5567:5504];
dut_yjp169 = dut_col[5567:5504];

gol_yjp170 = gol_col[5503:5440];
dut_yjp170 = dut_col[5503:5440];

gol_yjp171 = gol_col[5439:5376];
dut_yjp171 = dut_col[5439:5376];

gol_yjp172 = gol_col[5375:5312];
dut_yjp172 = dut_col[5375:5312];

gol_yjp173 = gol_col[5311:5248];
dut_yjp173 = dut_col[5311:5248];

gol_yjp174 = gol_col[5247:5184];
dut_yjp174 = dut_col[5247:5184];

gol_yjp175 = gol_col[5183:5120];
dut_yjp175 = dut_col[5183:5120];

gol_yjp176 = gol_col[5119:5056];
dut_yjp176 = dut_col[5119:5056];

gol_yjp177 = gol_col[5055:4992];
dut_yjp177 = dut_col[5055:4992];

gol_yjp178 = gol_col[4991:4928];
dut_yjp178 = dut_col[4991:4928];

gol_yjp179 = gol_col[4927:4864];
dut_yjp179 = dut_col[4927:4864];

gol_yjp180 = gol_col[4863:4800];
dut_yjp180 = dut_col[4863:4800];

gol_yjp181 = gol_col[4799:4736];
dut_yjp181 = dut_col[4799:4736];

gol_yjp182 = gol_col[4735:4672];
dut_yjp182 = dut_col[4735:4672];

gol_yjp183 = gol_col[4671:4608];
dut_yjp183 = dut_col[4671:4608];

gol_yjp184 = gol_col[4607:4544];
dut_yjp184 = dut_col[4607:4544];

gol_yjp185 = gol_col[4543:4480];
dut_yjp185 = dut_col[4543:4480];

gol_yjp186 = gol_col[4479:4416];
dut_yjp186 = dut_col[4479:4416];

gol_yjp187 = gol_col[4415:4352];
dut_yjp187 = dut_col[4415:4352];

gol_yjp188 = gol_col[4351:4288];
dut_yjp188 = dut_col[4351:4288];

gol_yjp189 = gol_col[4287:4224];
dut_yjp189 = dut_col[4287:4224];

gol_yjp190 = gol_col[4223:4160];
dut_yjp190 = dut_col[4223:4160];

gol_yjp191 = gol_col[4159:4096];
dut_yjp191 = dut_col[4159:4096];

gol_yjp192 = gol_col[4095:4032];
dut_yjp192 = dut_col[4095:4032];

gol_yjp193 = gol_col[4031:3968];
dut_yjp193 = dut_col[4031:3968];

gol_yjp194 = gol_col[3967:3904];
dut_yjp194 = dut_col[3967:3904];

gol_yjp195 = gol_col[3903:3840];
dut_yjp195 = dut_col[3903:3840];

gol_yjp196 = gol_col[3839:3776];
dut_yjp196 = dut_col[3839:3776];

gol_yjp197 = gol_col[3775:3712];
dut_yjp197 = dut_col[3775:3712];

gol_yjp198 = gol_col[3711:3648];
dut_yjp198 = dut_col[3711:3648];

gol_yjp199 = gol_col[3647:3584];
dut_yjp199 = dut_col[3647:3584];

gol_yjp200 = gol_col[3583:3520];
dut_yjp200 = dut_col[3583:3520];

gol_yjp201 = gol_col[3519:3456];
dut_yjp201 = dut_col[3519:3456];

gol_yjp202 = gol_col[3455:3392];
dut_yjp202 = dut_col[3455:3392];

gol_yjp203 = gol_col[3391:3328];
dut_yjp203 = dut_col[3391:3328];

gol_yjp204 = gol_col[3327:3264];
dut_yjp204 = dut_col[3327:3264];

gol_yjp205 = gol_col[3263:3200];
dut_yjp205 = dut_col[3263:3200];

gol_yjp206 = gol_col[3199:3136];
dut_yjp206 = dut_col[3199:3136];

gol_yjp207 = gol_col[3135:3072];
dut_yjp207 = dut_col[3135:3072];

gol_yjp208 = gol_col[3071:3008];
dut_yjp208 = dut_col[3071:3008];

gol_yjp209 = gol_col[3007:2944];
dut_yjp209 = dut_col[3007:2944];

gol_yjp210 = gol_col[2943:2880];
dut_yjp210 = dut_col[2943:2880];

gol_yjp211 = gol_col[2879:2816];
dut_yjp211 = dut_col[2879:2816];

gol_yjp212 = gol_col[2815:2752];
dut_yjp212 = dut_col[2815:2752];

gol_yjp213 = gol_col[2751:2688];
dut_yjp213 = dut_col[2751:2688];

gol_yjp214 = gol_col[2687:2624];
dut_yjp214 = dut_col[2687:2624];

gol_yjp215 = gol_col[2623:2560];
dut_yjp215 = dut_col[2623:2560];

gol_yjp216 = gol_col[2559:2496];
dut_yjp216 = dut_col[2559:2496];

gol_yjp217 = gol_col[2495:2432];
dut_yjp217 = dut_col[2495:2432];

gol_yjp218 = gol_col[2431:2368];
dut_yjp218 = dut_col[2431:2368];

gol_yjp219 = gol_col[2367:2304];
dut_yjp219 = dut_col[2367:2304];

gol_yjp220 = gol_col[2303:2240];
dut_yjp220 = dut_col[2303:2240];

gol_yjp221 = gol_col[2239:2176];
dut_yjp221 = dut_col[2239:2176];

gol_yjp222 = gol_col[2175:2112];
dut_yjp222 = dut_col[2175:2112];

gol_yjp223 = gol_col[2111:2048];
dut_yjp223 = dut_col[2111:2048];

gol_yjp224 = gol_col[2047:1984];
dut_yjp224 = dut_col[2047:1984];

gol_yjp225 = gol_col[1983:1920];
dut_yjp225 = dut_col[1983:1920];

gol_yjp226 = gol_col[1919:1856];
dut_yjp226 = dut_col[1919:1856];

gol_yjp227 = gol_col[1855:1792];
dut_yjp227 = dut_col[1855:1792];

gol_yjp228 = gol_col[1791:1728];
dut_yjp228 = dut_col[1791:1728];

gol_yjp229 = gol_col[1727:1664];
dut_yjp229 = dut_col[1727:1664];

gol_yjp230 = gol_col[1663:1600];
dut_yjp230 = dut_col[1663:1600];

gol_yjp231 = gol_col[1599:1536];
dut_yjp231 = dut_col[1599:1536];

gol_yjp232 = gol_col[1535:1472];
dut_yjp232 = dut_col[1535:1472];

gol_yjp233 = gol_col[1471:1408];
dut_yjp233 = dut_col[1471:1408];

gol_yjp234 = gol_col[1407:1344];
dut_yjp234 = dut_col[1407:1344];

gol_yjp235 = gol_col[1343:1280];
dut_yjp235 = dut_col[1343:1280];

gol_yjp236 = gol_col[1279:1216];
dut_yjp236 = dut_col[1279:1216];

gol_yjp237 = gol_col[1215:1152];
dut_yjp237 = dut_col[1215:1152];

gol_yjp238 = gol_col[1151:1088];
dut_yjp238 = dut_col[1151:1088];

gol_yjp239 = gol_col[1087:1024];
dut_yjp239 = dut_col[1087:1024];

gol_yjp240 = gol_col[1023:960];
dut_yjp240 = dut_col[1023:960];

gol_yjp241 = gol_col[959:896];
dut_yjp241 = dut_col[959:896];

gol_yjp242 = gol_col[895:832];
dut_yjp242 = dut_col[895:832];

gol_yjp243 = gol_col[831:768];
dut_yjp243 = dut_col[831:768];

gol_yjp244 = gol_col[767:704];
dut_yjp244 = dut_col[767:704];

gol_yjp245 = gol_col[703:640];
dut_yjp245 = dut_col[703:640];

gol_yjp246 = gol_col[639:576];
dut_yjp246 = dut_col[639:576];

gol_yjp247 = gol_col[575:512];
dut_yjp247 = dut_col[575:512];

gol_yjp248 = gol_col[511:448];
dut_yjp248 = dut_col[511:448];

gol_yjp249 = gol_col[447:384];
dut_yjp249 = dut_col[447:384];

gol_yjp250 = gol_col[383:320];
dut_yjp250 = dut_col[383:320];

gol_yjp251 = gol_col[319:256];
dut_yjp251 = dut_col[319:256];

gol_yjp252 = gol_col[255:192];
dut_yjp252 = dut_col[255:192];

gol_yjp253 = gol_col[191:128];
dut_yjp253 = dut_col[191:128];

gol_yjp254 = gol_col[127:64];
dut_yjp254 = dut_col[127:64];

gol_yjp255 = gol_col[63:0];
dut_yjp255 = dut_col[63:0];

gol_re_yjp0 = ieee754_to_fp(gol_yjp0[63:32]);
dut_re_yjp0 = ieee754_to_fp(dut_yjp0[63:32]);
gol_im_yjp0 = ieee754_to_fp(gol_yjp0[31:0]);
dut_im_yjp0 = ieee754_to_fp(dut_yjp0[31:0]);
gol_re_yjp1 = ieee754_to_fp(gol_yjp1[63:32]);
dut_re_yjp1 = ieee754_to_fp(dut_yjp1[63:32]);
gol_im_yjp1 = ieee754_to_fp(gol_yjp1[31:0]);
dut_im_yjp1 = ieee754_to_fp(dut_yjp1[31:0]);
gol_re_yjp2 = ieee754_to_fp(gol_yjp2[63:32]);
dut_re_yjp2 = ieee754_to_fp(dut_yjp2[63:32]);
gol_im_yjp2 = ieee754_to_fp(gol_yjp2[31:0]);
dut_im_yjp2 = ieee754_to_fp(dut_yjp2[31:0]);
gol_re_yjp3 = ieee754_to_fp(gol_yjp3[63:32]);
dut_re_yjp3 = ieee754_to_fp(dut_yjp3[63:32]);
gol_im_yjp3 = ieee754_to_fp(gol_yjp3[31:0]);
dut_im_yjp3 = ieee754_to_fp(dut_yjp3[31:0]);
gol_re_yjp4 = ieee754_to_fp(gol_yjp4[63:32]);
dut_re_yjp4 = ieee754_to_fp(dut_yjp4[63:32]);
gol_im_yjp4 = ieee754_to_fp(gol_yjp4[31:0]);
dut_im_yjp4 = ieee754_to_fp(dut_yjp4[31:0]);
gol_re_yjp5 = ieee754_to_fp(gol_yjp5[63:32]);
dut_re_yjp5 = ieee754_to_fp(dut_yjp5[63:32]);
gol_im_yjp5 = ieee754_to_fp(gol_yjp5[31:0]);
dut_im_yjp5 = ieee754_to_fp(dut_yjp5[31:0]);
gol_re_yjp6 = ieee754_to_fp(gol_yjp6[63:32]);
dut_re_yjp6 = ieee754_to_fp(dut_yjp6[63:32]);
gol_im_yjp6 = ieee754_to_fp(gol_yjp6[31:0]);
dut_im_yjp6 = ieee754_to_fp(dut_yjp6[31:0]);
gol_re_yjp7 = ieee754_to_fp(gol_yjp7[63:32]);
dut_re_yjp7 = ieee754_to_fp(dut_yjp7[63:32]);
gol_im_yjp7 = ieee754_to_fp(gol_yjp7[31:0]);
dut_im_yjp7 = ieee754_to_fp(dut_yjp7[31:0]);
gol_re_yjp8 = ieee754_to_fp(gol_yjp8[63:32]);
dut_re_yjp8 = ieee754_to_fp(dut_yjp8[63:32]);
gol_im_yjp8 = ieee754_to_fp(gol_yjp8[31:0]);
dut_im_yjp8 = ieee754_to_fp(dut_yjp8[31:0]);
gol_re_yjp9 = ieee754_to_fp(gol_yjp9[63:32]);
dut_re_yjp9 = ieee754_to_fp(dut_yjp9[63:32]);
gol_im_yjp9 = ieee754_to_fp(gol_yjp9[31:0]);
dut_im_yjp9 = ieee754_to_fp(dut_yjp9[31:0]);
gol_re_yjp10 = ieee754_to_fp(gol_yjp10[63:32]);
dut_re_yjp10 = ieee754_to_fp(dut_yjp10[63:32]);
gol_im_yjp10 = ieee754_to_fp(gol_yjp10[31:0]);
dut_im_yjp10 = ieee754_to_fp(dut_yjp10[31:0]);
gol_re_yjp11 = ieee754_to_fp(gol_yjp11[63:32]);
dut_re_yjp11 = ieee754_to_fp(dut_yjp11[63:32]);
gol_im_yjp11 = ieee754_to_fp(gol_yjp11[31:0]);
dut_im_yjp11 = ieee754_to_fp(dut_yjp11[31:0]);
gol_re_yjp12 = ieee754_to_fp(gol_yjp12[63:32]);
dut_re_yjp12 = ieee754_to_fp(dut_yjp12[63:32]);
gol_im_yjp12 = ieee754_to_fp(gol_yjp12[31:0]);
dut_im_yjp12 = ieee754_to_fp(dut_yjp12[31:0]);
gol_re_yjp13 = ieee754_to_fp(gol_yjp13[63:32]);
dut_re_yjp13 = ieee754_to_fp(dut_yjp13[63:32]);
gol_im_yjp13 = ieee754_to_fp(gol_yjp13[31:0]);
dut_im_yjp13 = ieee754_to_fp(dut_yjp13[31:0]);
gol_re_yjp14 = ieee754_to_fp(gol_yjp14[63:32]);
dut_re_yjp14 = ieee754_to_fp(dut_yjp14[63:32]);
gol_im_yjp14 = ieee754_to_fp(gol_yjp14[31:0]);
dut_im_yjp14 = ieee754_to_fp(dut_yjp14[31:0]);
gol_re_yjp15 = ieee754_to_fp(gol_yjp15[63:32]);
dut_re_yjp15 = ieee754_to_fp(dut_yjp15[63:32]);
gol_im_yjp15 = ieee754_to_fp(gol_yjp15[31:0]);
dut_im_yjp15 = ieee754_to_fp(dut_yjp15[31:0]);
gol_re_yjp16 = ieee754_to_fp(gol_yjp16[63:32]);
dut_re_yjp16 = ieee754_to_fp(dut_yjp16[63:32]);
gol_im_yjp16 = ieee754_to_fp(gol_yjp16[31:0]);
dut_im_yjp16 = ieee754_to_fp(dut_yjp16[31:0]);
gol_re_yjp17 = ieee754_to_fp(gol_yjp17[63:32]);
dut_re_yjp17 = ieee754_to_fp(dut_yjp17[63:32]);
gol_im_yjp17 = ieee754_to_fp(gol_yjp17[31:0]);
dut_im_yjp17 = ieee754_to_fp(dut_yjp17[31:0]);
gol_re_yjp18 = ieee754_to_fp(gol_yjp18[63:32]);
dut_re_yjp18 = ieee754_to_fp(dut_yjp18[63:32]);
gol_im_yjp18 = ieee754_to_fp(gol_yjp18[31:0]);
dut_im_yjp18 = ieee754_to_fp(dut_yjp18[31:0]);
gol_re_yjp19 = ieee754_to_fp(gol_yjp19[63:32]);
dut_re_yjp19 = ieee754_to_fp(dut_yjp19[63:32]);
gol_im_yjp19 = ieee754_to_fp(gol_yjp19[31:0]);
dut_im_yjp19 = ieee754_to_fp(dut_yjp19[31:0]);
gol_re_yjp20 = ieee754_to_fp(gol_yjp20[63:32]);
dut_re_yjp20 = ieee754_to_fp(dut_yjp20[63:32]);
gol_im_yjp20 = ieee754_to_fp(gol_yjp20[31:0]);
dut_im_yjp20 = ieee754_to_fp(dut_yjp20[31:0]);
gol_re_yjp21 = ieee754_to_fp(gol_yjp21[63:32]);
dut_re_yjp21 = ieee754_to_fp(dut_yjp21[63:32]);
gol_im_yjp21 = ieee754_to_fp(gol_yjp21[31:0]);
dut_im_yjp21 = ieee754_to_fp(dut_yjp21[31:0]);
gol_re_yjp22 = ieee754_to_fp(gol_yjp22[63:32]);
dut_re_yjp22 = ieee754_to_fp(dut_yjp22[63:32]);
gol_im_yjp22 = ieee754_to_fp(gol_yjp22[31:0]);
dut_im_yjp22 = ieee754_to_fp(dut_yjp22[31:0]);
gol_re_yjp23 = ieee754_to_fp(gol_yjp23[63:32]);
dut_re_yjp23 = ieee754_to_fp(dut_yjp23[63:32]);
gol_im_yjp23 = ieee754_to_fp(gol_yjp23[31:0]);
dut_im_yjp23 = ieee754_to_fp(dut_yjp23[31:0]);
gol_re_yjp24 = ieee754_to_fp(gol_yjp24[63:32]);
dut_re_yjp24 = ieee754_to_fp(dut_yjp24[63:32]);
gol_im_yjp24 = ieee754_to_fp(gol_yjp24[31:0]);
dut_im_yjp24 = ieee754_to_fp(dut_yjp24[31:0]);
gol_re_yjp25 = ieee754_to_fp(gol_yjp25[63:32]);
dut_re_yjp25 = ieee754_to_fp(dut_yjp25[63:32]);
gol_im_yjp25 = ieee754_to_fp(gol_yjp25[31:0]);
dut_im_yjp25 = ieee754_to_fp(dut_yjp25[31:0]);
gol_re_yjp26 = ieee754_to_fp(gol_yjp26[63:32]);
dut_re_yjp26 = ieee754_to_fp(dut_yjp26[63:32]);
gol_im_yjp26 = ieee754_to_fp(gol_yjp26[31:0]);
dut_im_yjp26 = ieee754_to_fp(dut_yjp26[31:0]);
gol_re_yjp27 = ieee754_to_fp(gol_yjp27[63:32]);
dut_re_yjp27 = ieee754_to_fp(dut_yjp27[63:32]);
gol_im_yjp27 = ieee754_to_fp(gol_yjp27[31:0]);
dut_im_yjp27 = ieee754_to_fp(dut_yjp27[31:0]);
gol_re_yjp28 = ieee754_to_fp(gol_yjp28[63:32]);
dut_re_yjp28 = ieee754_to_fp(dut_yjp28[63:32]);
gol_im_yjp28 = ieee754_to_fp(gol_yjp28[31:0]);
dut_im_yjp28 = ieee754_to_fp(dut_yjp28[31:0]);
gol_re_yjp29 = ieee754_to_fp(gol_yjp29[63:32]);
dut_re_yjp29 = ieee754_to_fp(dut_yjp29[63:32]);
gol_im_yjp29 = ieee754_to_fp(gol_yjp29[31:0]);
dut_im_yjp29 = ieee754_to_fp(dut_yjp29[31:0]);
gol_re_yjp30 = ieee754_to_fp(gol_yjp30[63:32]);
dut_re_yjp30 = ieee754_to_fp(dut_yjp30[63:32]);
gol_im_yjp30 = ieee754_to_fp(gol_yjp30[31:0]);
dut_im_yjp30 = ieee754_to_fp(dut_yjp30[31:0]);
gol_re_yjp31 = ieee754_to_fp(gol_yjp31[63:32]);
dut_re_yjp31 = ieee754_to_fp(dut_yjp31[63:32]);
gol_im_yjp31 = ieee754_to_fp(gol_yjp31[31:0]);
dut_im_yjp31 = ieee754_to_fp(dut_yjp31[31:0]);
gol_re_yjp32 = ieee754_to_fp(gol_yjp32[63:32]);
dut_re_yjp32 = ieee754_to_fp(dut_yjp32[63:32]);
gol_im_yjp32 = ieee754_to_fp(gol_yjp32[31:0]);
dut_im_yjp32 = ieee754_to_fp(dut_yjp32[31:0]);
gol_re_yjp33 = ieee754_to_fp(gol_yjp33[63:32]);
dut_re_yjp33 = ieee754_to_fp(dut_yjp33[63:32]);
gol_im_yjp33 = ieee754_to_fp(gol_yjp33[31:0]);
dut_im_yjp33 = ieee754_to_fp(dut_yjp33[31:0]);
gol_re_yjp34 = ieee754_to_fp(gol_yjp34[63:32]);
dut_re_yjp34 = ieee754_to_fp(dut_yjp34[63:32]);
gol_im_yjp34 = ieee754_to_fp(gol_yjp34[31:0]);
dut_im_yjp34 = ieee754_to_fp(dut_yjp34[31:0]);
gol_re_yjp35 = ieee754_to_fp(gol_yjp35[63:32]);
dut_re_yjp35 = ieee754_to_fp(dut_yjp35[63:32]);
gol_im_yjp35 = ieee754_to_fp(gol_yjp35[31:0]);
dut_im_yjp35 = ieee754_to_fp(dut_yjp35[31:0]);
gol_re_yjp36 = ieee754_to_fp(gol_yjp36[63:32]);
dut_re_yjp36 = ieee754_to_fp(dut_yjp36[63:32]);
gol_im_yjp36 = ieee754_to_fp(gol_yjp36[31:0]);
dut_im_yjp36 = ieee754_to_fp(dut_yjp36[31:0]);
gol_re_yjp37 = ieee754_to_fp(gol_yjp37[63:32]);
dut_re_yjp37 = ieee754_to_fp(dut_yjp37[63:32]);
gol_im_yjp37 = ieee754_to_fp(gol_yjp37[31:0]);
dut_im_yjp37 = ieee754_to_fp(dut_yjp37[31:0]);
gol_re_yjp38 = ieee754_to_fp(gol_yjp38[63:32]);
dut_re_yjp38 = ieee754_to_fp(dut_yjp38[63:32]);
gol_im_yjp38 = ieee754_to_fp(gol_yjp38[31:0]);
dut_im_yjp38 = ieee754_to_fp(dut_yjp38[31:0]);
gol_re_yjp39 = ieee754_to_fp(gol_yjp39[63:32]);
dut_re_yjp39 = ieee754_to_fp(dut_yjp39[63:32]);
gol_im_yjp39 = ieee754_to_fp(gol_yjp39[31:0]);
dut_im_yjp39 = ieee754_to_fp(dut_yjp39[31:0]);
gol_re_yjp40 = ieee754_to_fp(gol_yjp40[63:32]);
dut_re_yjp40 = ieee754_to_fp(dut_yjp40[63:32]);
gol_im_yjp40 = ieee754_to_fp(gol_yjp40[31:0]);
dut_im_yjp40 = ieee754_to_fp(dut_yjp40[31:0]);
gol_re_yjp41 = ieee754_to_fp(gol_yjp41[63:32]);
dut_re_yjp41 = ieee754_to_fp(dut_yjp41[63:32]);
gol_im_yjp41 = ieee754_to_fp(gol_yjp41[31:0]);
dut_im_yjp41 = ieee754_to_fp(dut_yjp41[31:0]);
gol_re_yjp42 = ieee754_to_fp(gol_yjp42[63:32]);
dut_re_yjp42 = ieee754_to_fp(dut_yjp42[63:32]);
gol_im_yjp42 = ieee754_to_fp(gol_yjp42[31:0]);
dut_im_yjp42 = ieee754_to_fp(dut_yjp42[31:0]);
gol_re_yjp43 = ieee754_to_fp(gol_yjp43[63:32]);
dut_re_yjp43 = ieee754_to_fp(dut_yjp43[63:32]);
gol_im_yjp43 = ieee754_to_fp(gol_yjp43[31:0]);
dut_im_yjp43 = ieee754_to_fp(dut_yjp43[31:0]);
gol_re_yjp44 = ieee754_to_fp(gol_yjp44[63:32]);
dut_re_yjp44 = ieee754_to_fp(dut_yjp44[63:32]);
gol_im_yjp44 = ieee754_to_fp(gol_yjp44[31:0]);
dut_im_yjp44 = ieee754_to_fp(dut_yjp44[31:0]);
gol_re_yjp45 = ieee754_to_fp(gol_yjp45[63:32]);
dut_re_yjp45 = ieee754_to_fp(dut_yjp45[63:32]);
gol_im_yjp45 = ieee754_to_fp(gol_yjp45[31:0]);
dut_im_yjp45 = ieee754_to_fp(dut_yjp45[31:0]);
gol_re_yjp46 = ieee754_to_fp(gol_yjp46[63:32]);
dut_re_yjp46 = ieee754_to_fp(dut_yjp46[63:32]);
gol_im_yjp46 = ieee754_to_fp(gol_yjp46[31:0]);
dut_im_yjp46 = ieee754_to_fp(dut_yjp46[31:0]);
gol_re_yjp47 = ieee754_to_fp(gol_yjp47[63:32]);
dut_re_yjp47 = ieee754_to_fp(dut_yjp47[63:32]);
gol_im_yjp47 = ieee754_to_fp(gol_yjp47[31:0]);
dut_im_yjp47 = ieee754_to_fp(dut_yjp47[31:0]);
gol_re_yjp48 = ieee754_to_fp(gol_yjp48[63:32]);
dut_re_yjp48 = ieee754_to_fp(dut_yjp48[63:32]);
gol_im_yjp48 = ieee754_to_fp(gol_yjp48[31:0]);
dut_im_yjp48 = ieee754_to_fp(dut_yjp48[31:0]);
gol_re_yjp49 = ieee754_to_fp(gol_yjp49[63:32]);
dut_re_yjp49 = ieee754_to_fp(dut_yjp49[63:32]);
gol_im_yjp49 = ieee754_to_fp(gol_yjp49[31:0]);
dut_im_yjp49 = ieee754_to_fp(dut_yjp49[31:0]);
gol_re_yjp50 = ieee754_to_fp(gol_yjp50[63:32]);
dut_re_yjp50 = ieee754_to_fp(dut_yjp50[63:32]);
gol_im_yjp50 = ieee754_to_fp(gol_yjp50[31:0]);
dut_im_yjp50 = ieee754_to_fp(dut_yjp50[31:0]);
gol_re_yjp51 = ieee754_to_fp(gol_yjp51[63:32]);
dut_re_yjp51 = ieee754_to_fp(dut_yjp51[63:32]);
gol_im_yjp51 = ieee754_to_fp(gol_yjp51[31:0]);
dut_im_yjp51 = ieee754_to_fp(dut_yjp51[31:0]);
gol_re_yjp52 = ieee754_to_fp(gol_yjp52[63:32]);
dut_re_yjp52 = ieee754_to_fp(dut_yjp52[63:32]);
gol_im_yjp52 = ieee754_to_fp(gol_yjp52[31:0]);
dut_im_yjp52 = ieee754_to_fp(dut_yjp52[31:0]);
gol_re_yjp53 = ieee754_to_fp(gol_yjp53[63:32]);
dut_re_yjp53 = ieee754_to_fp(dut_yjp53[63:32]);
gol_im_yjp53 = ieee754_to_fp(gol_yjp53[31:0]);
dut_im_yjp53 = ieee754_to_fp(dut_yjp53[31:0]);
gol_re_yjp54 = ieee754_to_fp(gol_yjp54[63:32]);
dut_re_yjp54 = ieee754_to_fp(dut_yjp54[63:32]);
gol_im_yjp54 = ieee754_to_fp(gol_yjp54[31:0]);
dut_im_yjp54 = ieee754_to_fp(dut_yjp54[31:0]);
gol_re_yjp55 = ieee754_to_fp(gol_yjp55[63:32]);
dut_re_yjp55 = ieee754_to_fp(dut_yjp55[63:32]);
gol_im_yjp55 = ieee754_to_fp(gol_yjp55[31:0]);
dut_im_yjp55 = ieee754_to_fp(dut_yjp55[31:0]);
gol_re_yjp56 = ieee754_to_fp(gol_yjp56[63:32]);
dut_re_yjp56 = ieee754_to_fp(dut_yjp56[63:32]);
gol_im_yjp56 = ieee754_to_fp(gol_yjp56[31:0]);
dut_im_yjp56 = ieee754_to_fp(dut_yjp56[31:0]);
gol_re_yjp57 = ieee754_to_fp(gol_yjp57[63:32]);
dut_re_yjp57 = ieee754_to_fp(dut_yjp57[63:32]);
gol_im_yjp57 = ieee754_to_fp(gol_yjp57[31:0]);
dut_im_yjp57 = ieee754_to_fp(dut_yjp57[31:0]);
gol_re_yjp58 = ieee754_to_fp(gol_yjp58[63:32]);
dut_re_yjp58 = ieee754_to_fp(dut_yjp58[63:32]);
gol_im_yjp58 = ieee754_to_fp(gol_yjp58[31:0]);
dut_im_yjp58 = ieee754_to_fp(dut_yjp58[31:0]);
gol_re_yjp59 = ieee754_to_fp(gol_yjp59[63:32]);
dut_re_yjp59 = ieee754_to_fp(dut_yjp59[63:32]);
gol_im_yjp59 = ieee754_to_fp(gol_yjp59[31:0]);
dut_im_yjp59 = ieee754_to_fp(dut_yjp59[31:0]);
gol_re_yjp60 = ieee754_to_fp(gol_yjp60[63:32]);
dut_re_yjp60 = ieee754_to_fp(dut_yjp60[63:32]);
gol_im_yjp60 = ieee754_to_fp(gol_yjp60[31:0]);
dut_im_yjp60 = ieee754_to_fp(dut_yjp60[31:0]);
gol_re_yjp61 = ieee754_to_fp(gol_yjp61[63:32]);
dut_re_yjp61 = ieee754_to_fp(dut_yjp61[63:32]);
gol_im_yjp61 = ieee754_to_fp(gol_yjp61[31:0]);
dut_im_yjp61 = ieee754_to_fp(dut_yjp61[31:0]);
gol_re_yjp62 = ieee754_to_fp(gol_yjp62[63:32]);
dut_re_yjp62 = ieee754_to_fp(dut_yjp62[63:32]);
gol_im_yjp62 = ieee754_to_fp(gol_yjp62[31:0]);
dut_im_yjp62 = ieee754_to_fp(dut_yjp62[31:0]);
gol_re_yjp63 = ieee754_to_fp(gol_yjp63[63:32]);
dut_re_yjp63 = ieee754_to_fp(dut_yjp63[63:32]);
gol_im_yjp63 = ieee754_to_fp(gol_yjp63[31:0]);
dut_im_yjp63 = ieee754_to_fp(dut_yjp63[31:0]);
gol_re_yjp64 = ieee754_to_fp(gol_yjp64[63:32]);
dut_re_yjp64 = ieee754_to_fp(dut_yjp64[63:32]);
gol_im_yjp64 = ieee754_to_fp(gol_yjp64[31:0]);
dut_im_yjp64 = ieee754_to_fp(dut_yjp64[31:0]);
gol_re_yjp65 = ieee754_to_fp(gol_yjp65[63:32]);
dut_re_yjp65 = ieee754_to_fp(dut_yjp65[63:32]);
gol_im_yjp65 = ieee754_to_fp(gol_yjp65[31:0]);
dut_im_yjp65 = ieee754_to_fp(dut_yjp65[31:0]);
gol_re_yjp66 = ieee754_to_fp(gol_yjp66[63:32]);
dut_re_yjp66 = ieee754_to_fp(dut_yjp66[63:32]);
gol_im_yjp66 = ieee754_to_fp(gol_yjp66[31:0]);
dut_im_yjp66 = ieee754_to_fp(dut_yjp66[31:0]);
gol_re_yjp67 = ieee754_to_fp(gol_yjp67[63:32]);
dut_re_yjp67 = ieee754_to_fp(dut_yjp67[63:32]);
gol_im_yjp67 = ieee754_to_fp(gol_yjp67[31:0]);
dut_im_yjp67 = ieee754_to_fp(dut_yjp67[31:0]);
gol_re_yjp68 = ieee754_to_fp(gol_yjp68[63:32]);
dut_re_yjp68 = ieee754_to_fp(dut_yjp68[63:32]);
gol_im_yjp68 = ieee754_to_fp(gol_yjp68[31:0]);
dut_im_yjp68 = ieee754_to_fp(dut_yjp68[31:0]);
gol_re_yjp69 = ieee754_to_fp(gol_yjp69[63:32]);
dut_re_yjp69 = ieee754_to_fp(dut_yjp69[63:32]);
gol_im_yjp69 = ieee754_to_fp(gol_yjp69[31:0]);
dut_im_yjp69 = ieee754_to_fp(dut_yjp69[31:0]);
gol_re_yjp70 = ieee754_to_fp(gol_yjp70[63:32]);
dut_re_yjp70 = ieee754_to_fp(dut_yjp70[63:32]);
gol_im_yjp70 = ieee754_to_fp(gol_yjp70[31:0]);
dut_im_yjp70 = ieee754_to_fp(dut_yjp70[31:0]);
gol_re_yjp71 = ieee754_to_fp(gol_yjp71[63:32]);
dut_re_yjp71 = ieee754_to_fp(dut_yjp71[63:32]);
gol_im_yjp71 = ieee754_to_fp(gol_yjp71[31:0]);
dut_im_yjp71 = ieee754_to_fp(dut_yjp71[31:0]);
gol_re_yjp72 = ieee754_to_fp(gol_yjp72[63:32]);
dut_re_yjp72 = ieee754_to_fp(dut_yjp72[63:32]);
gol_im_yjp72 = ieee754_to_fp(gol_yjp72[31:0]);
dut_im_yjp72 = ieee754_to_fp(dut_yjp72[31:0]);
gol_re_yjp73 = ieee754_to_fp(gol_yjp73[63:32]);
dut_re_yjp73 = ieee754_to_fp(dut_yjp73[63:32]);
gol_im_yjp73 = ieee754_to_fp(gol_yjp73[31:0]);
dut_im_yjp73 = ieee754_to_fp(dut_yjp73[31:0]);
gol_re_yjp74 = ieee754_to_fp(gol_yjp74[63:32]);
dut_re_yjp74 = ieee754_to_fp(dut_yjp74[63:32]);
gol_im_yjp74 = ieee754_to_fp(gol_yjp74[31:0]);
dut_im_yjp74 = ieee754_to_fp(dut_yjp74[31:0]);
gol_re_yjp75 = ieee754_to_fp(gol_yjp75[63:32]);
dut_re_yjp75 = ieee754_to_fp(dut_yjp75[63:32]);
gol_im_yjp75 = ieee754_to_fp(gol_yjp75[31:0]);
dut_im_yjp75 = ieee754_to_fp(dut_yjp75[31:0]);
gol_re_yjp76 = ieee754_to_fp(gol_yjp76[63:32]);
dut_re_yjp76 = ieee754_to_fp(dut_yjp76[63:32]);
gol_im_yjp76 = ieee754_to_fp(gol_yjp76[31:0]);
dut_im_yjp76 = ieee754_to_fp(dut_yjp76[31:0]);
gol_re_yjp77 = ieee754_to_fp(gol_yjp77[63:32]);
dut_re_yjp77 = ieee754_to_fp(dut_yjp77[63:32]);
gol_im_yjp77 = ieee754_to_fp(gol_yjp77[31:0]);
dut_im_yjp77 = ieee754_to_fp(dut_yjp77[31:0]);
gol_re_yjp78 = ieee754_to_fp(gol_yjp78[63:32]);
dut_re_yjp78 = ieee754_to_fp(dut_yjp78[63:32]);
gol_im_yjp78 = ieee754_to_fp(gol_yjp78[31:0]);
dut_im_yjp78 = ieee754_to_fp(dut_yjp78[31:0]);
gol_re_yjp79 = ieee754_to_fp(gol_yjp79[63:32]);
dut_re_yjp79 = ieee754_to_fp(dut_yjp79[63:32]);
gol_im_yjp79 = ieee754_to_fp(gol_yjp79[31:0]);
dut_im_yjp79 = ieee754_to_fp(dut_yjp79[31:0]);
gol_re_yjp80 = ieee754_to_fp(gol_yjp80[63:32]);
dut_re_yjp80 = ieee754_to_fp(dut_yjp80[63:32]);
gol_im_yjp80 = ieee754_to_fp(gol_yjp80[31:0]);
dut_im_yjp80 = ieee754_to_fp(dut_yjp80[31:0]);
gol_re_yjp81 = ieee754_to_fp(gol_yjp81[63:32]);
dut_re_yjp81 = ieee754_to_fp(dut_yjp81[63:32]);
gol_im_yjp81 = ieee754_to_fp(gol_yjp81[31:0]);
dut_im_yjp81 = ieee754_to_fp(dut_yjp81[31:0]);
gol_re_yjp82 = ieee754_to_fp(gol_yjp82[63:32]);
dut_re_yjp82 = ieee754_to_fp(dut_yjp82[63:32]);
gol_im_yjp82 = ieee754_to_fp(gol_yjp82[31:0]);
dut_im_yjp82 = ieee754_to_fp(dut_yjp82[31:0]);
gol_re_yjp83 = ieee754_to_fp(gol_yjp83[63:32]);
dut_re_yjp83 = ieee754_to_fp(dut_yjp83[63:32]);
gol_im_yjp83 = ieee754_to_fp(gol_yjp83[31:0]);
dut_im_yjp83 = ieee754_to_fp(dut_yjp83[31:0]);
gol_re_yjp84 = ieee754_to_fp(gol_yjp84[63:32]);
dut_re_yjp84 = ieee754_to_fp(dut_yjp84[63:32]);
gol_im_yjp84 = ieee754_to_fp(gol_yjp84[31:0]);
dut_im_yjp84 = ieee754_to_fp(dut_yjp84[31:0]);
gol_re_yjp85 = ieee754_to_fp(gol_yjp85[63:32]);
dut_re_yjp85 = ieee754_to_fp(dut_yjp85[63:32]);
gol_im_yjp85 = ieee754_to_fp(gol_yjp85[31:0]);
dut_im_yjp85 = ieee754_to_fp(dut_yjp85[31:0]);
gol_re_yjp86 = ieee754_to_fp(gol_yjp86[63:32]);
dut_re_yjp86 = ieee754_to_fp(dut_yjp86[63:32]);
gol_im_yjp86 = ieee754_to_fp(gol_yjp86[31:0]);
dut_im_yjp86 = ieee754_to_fp(dut_yjp86[31:0]);
gol_re_yjp87 = ieee754_to_fp(gol_yjp87[63:32]);
dut_re_yjp87 = ieee754_to_fp(dut_yjp87[63:32]);
gol_im_yjp87 = ieee754_to_fp(gol_yjp87[31:0]);
dut_im_yjp87 = ieee754_to_fp(dut_yjp87[31:0]);
gol_re_yjp88 = ieee754_to_fp(gol_yjp88[63:32]);
dut_re_yjp88 = ieee754_to_fp(dut_yjp88[63:32]);
gol_im_yjp88 = ieee754_to_fp(gol_yjp88[31:0]);
dut_im_yjp88 = ieee754_to_fp(dut_yjp88[31:0]);
gol_re_yjp89 = ieee754_to_fp(gol_yjp89[63:32]);
dut_re_yjp89 = ieee754_to_fp(dut_yjp89[63:32]);
gol_im_yjp89 = ieee754_to_fp(gol_yjp89[31:0]);
dut_im_yjp89 = ieee754_to_fp(dut_yjp89[31:0]);
gol_re_yjp90 = ieee754_to_fp(gol_yjp90[63:32]);
dut_re_yjp90 = ieee754_to_fp(dut_yjp90[63:32]);
gol_im_yjp90 = ieee754_to_fp(gol_yjp90[31:0]);
dut_im_yjp90 = ieee754_to_fp(dut_yjp90[31:0]);
gol_re_yjp91 = ieee754_to_fp(gol_yjp91[63:32]);
dut_re_yjp91 = ieee754_to_fp(dut_yjp91[63:32]);
gol_im_yjp91 = ieee754_to_fp(gol_yjp91[31:0]);
dut_im_yjp91 = ieee754_to_fp(dut_yjp91[31:0]);
gol_re_yjp92 = ieee754_to_fp(gol_yjp92[63:32]);
dut_re_yjp92 = ieee754_to_fp(dut_yjp92[63:32]);
gol_im_yjp92 = ieee754_to_fp(gol_yjp92[31:0]);
dut_im_yjp92 = ieee754_to_fp(dut_yjp92[31:0]);
gol_re_yjp93 = ieee754_to_fp(gol_yjp93[63:32]);
dut_re_yjp93 = ieee754_to_fp(dut_yjp93[63:32]);
gol_im_yjp93 = ieee754_to_fp(gol_yjp93[31:0]);
dut_im_yjp93 = ieee754_to_fp(dut_yjp93[31:0]);
gol_re_yjp94 = ieee754_to_fp(gol_yjp94[63:32]);
dut_re_yjp94 = ieee754_to_fp(dut_yjp94[63:32]);
gol_im_yjp94 = ieee754_to_fp(gol_yjp94[31:0]);
dut_im_yjp94 = ieee754_to_fp(dut_yjp94[31:0]);
gol_re_yjp95 = ieee754_to_fp(gol_yjp95[63:32]);
dut_re_yjp95 = ieee754_to_fp(dut_yjp95[63:32]);
gol_im_yjp95 = ieee754_to_fp(gol_yjp95[31:0]);
dut_im_yjp95 = ieee754_to_fp(dut_yjp95[31:0]);
gol_re_yjp96 = ieee754_to_fp(gol_yjp96[63:32]);
dut_re_yjp96 = ieee754_to_fp(dut_yjp96[63:32]);
gol_im_yjp96 = ieee754_to_fp(gol_yjp96[31:0]);
dut_im_yjp96 = ieee754_to_fp(dut_yjp96[31:0]);
gol_re_yjp97 = ieee754_to_fp(gol_yjp97[63:32]);
dut_re_yjp97 = ieee754_to_fp(dut_yjp97[63:32]);
gol_im_yjp97 = ieee754_to_fp(gol_yjp97[31:0]);
dut_im_yjp97 = ieee754_to_fp(dut_yjp97[31:0]);
gol_re_yjp98 = ieee754_to_fp(gol_yjp98[63:32]);
dut_re_yjp98 = ieee754_to_fp(dut_yjp98[63:32]);
gol_im_yjp98 = ieee754_to_fp(gol_yjp98[31:0]);
dut_im_yjp98 = ieee754_to_fp(dut_yjp98[31:0]);
gol_re_yjp99 = ieee754_to_fp(gol_yjp99[63:32]);
dut_re_yjp99 = ieee754_to_fp(dut_yjp99[63:32]);
gol_im_yjp99 = ieee754_to_fp(gol_yjp99[31:0]);
dut_im_yjp99 = ieee754_to_fp(dut_yjp99[31:0]);
gol_re_yjp100 = ieee754_to_fp(gol_yjp100[63:32]);
dut_re_yjp100 = ieee754_to_fp(dut_yjp100[63:32]);
gol_im_yjp100 = ieee754_to_fp(gol_yjp100[31:0]);
dut_im_yjp100 = ieee754_to_fp(dut_yjp100[31:0]);
gol_re_yjp101 = ieee754_to_fp(gol_yjp101[63:32]);
dut_re_yjp101 = ieee754_to_fp(dut_yjp101[63:32]);
gol_im_yjp101 = ieee754_to_fp(gol_yjp101[31:0]);
dut_im_yjp101 = ieee754_to_fp(dut_yjp101[31:0]);
gol_re_yjp102 = ieee754_to_fp(gol_yjp102[63:32]);
dut_re_yjp102 = ieee754_to_fp(dut_yjp102[63:32]);
gol_im_yjp102 = ieee754_to_fp(gol_yjp102[31:0]);
dut_im_yjp102 = ieee754_to_fp(dut_yjp102[31:0]);
gol_re_yjp103 = ieee754_to_fp(gol_yjp103[63:32]);
dut_re_yjp103 = ieee754_to_fp(dut_yjp103[63:32]);
gol_im_yjp103 = ieee754_to_fp(gol_yjp103[31:0]);
dut_im_yjp103 = ieee754_to_fp(dut_yjp103[31:0]);
gol_re_yjp104 = ieee754_to_fp(gol_yjp104[63:32]);
dut_re_yjp104 = ieee754_to_fp(dut_yjp104[63:32]);
gol_im_yjp104 = ieee754_to_fp(gol_yjp104[31:0]);
dut_im_yjp104 = ieee754_to_fp(dut_yjp104[31:0]);
gol_re_yjp105 = ieee754_to_fp(gol_yjp105[63:32]);
dut_re_yjp105 = ieee754_to_fp(dut_yjp105[63:32]);
gol_im_yjp105 = ieee754_to_fp(gol_yjp105[31:0]);
dut_im_yjp105 = ieee754_to_fp(dut_yjp105[31:0]);
gol_re_yjp106 = ieee754_to_fp(gol_yjp106[63:32]);
dut_re_yjp106 = ieee754_to_fp(dut_yjp106[63:32]);
gol_im_yjp106 = ieee754_to_fp(gol_yjp106[31:0]);
dut_im_yjp106 = ieee754_to_fp(dut_yjp106[31:0]);
gol_re_yjp107 = ieee754_to_fp(gol_yjp107[63:32]);
dut_re_yjp107 = ieee754_to_fp(dut_yjp107[63:32]);
gol_im_yjp107 = ieee754_to_fp(gol_yjp107[31:0]);
dut_im_yjp107 = ieee754_to_fp(dut_yjp107[31:0]);
gol_re_yjp108 = ieee754_to_fp(gol_yjp108[63:32]);
dut_re_yjp108 = ieee754_to_fp(dut_yjp108[63:32]);
gol_im_yjp108 = ieee754_to_fp(gol_yjp108[31:0]);
dut_im_yjp108 = ieee754_to_fp(dut_yjp108[31:0]);
gol_re_yjp109 = ieee754_to_fp(gol_yjp109[63:32]);
dut_re_yjp109 = ieee754_to_fp(dut_yjp109[63:32]);
gol_im_yjp109 = ieee754_to_fp(gol_yjp109[31:0]);
dut_im_yjp109 = ieee754_to_fp(dut_yjp109[31:0]);
gol_re_yjp110 = ieee754_to_fp(gol_yjp110[63:32]);
dut_re_yjp110 = ieee754_to_fp(dut_yjp110[63:32]);
gol_im_yjp110 = ieee754_to_fp(gol_yjp110[31:0]);
dut_im_yjp110 = ieee754_to_fp(dut_yjp110[31:0]);
gol_re_yjp111 = ieee754_to_fp(gol_yjp111[63:32]);
dut_re_yjp111 = ieee754_to_fp(dut_yjp111[63:32]);
gol_im_yjp111 = ieee754_to_fp(gol_yjp111[31:0]);
dut_im_yjp111 = ieee754_to_fp(dut_yjp111[31:0]);
gol_re_yjp112 = ieee754_to_fp(gol_yjp112[63:32]);
dut_re_yjp112 = ieee754_to_fp(dut_yjp112[63:32]);
gol_im_yjp112 = ieee754_to_fp(gol_yjp112[31:0]);
dut_im_yjp112 = ieee754_to_fp(dut_yjp112[31:0]);
gol_re_yjp113 = ieee754_to_fp(gol_yjp113[63:32]);
dut_re_yjp113 = ieee754_to_fp(dut_yjp113[63:32]);
gol_im_yjp113 = ieee754_to_fp(gol_yjp113[31:0]);
dut_im_yjp113 = ieee754_to_fp(dut_yjp113[31:0]);
gol_re_yjp114 = ieee754_to_fp(gol_yjp114[63:32]);
dut_re_yjp114 = ieee754_to_fp(dut_yjp114[63:32]);
gol_im_yjp114 = ieee754_to_fp(gol_yjp114[31:0]);
dut_im_yjp114 = ieee754_to_fp(dut_yjp114[31:0]);
gol_re_yjp115 = ieee754_to_fp(gol_yjp115[63:32]);
dut_re_yjp115 = ieee754_to_fp(dut_yjp115[63:32]);
gol_im_yjp115 = ieee754_to_fp(gol_yjp115[31:0]);
dut_im_yjp115 = ieee754_to_fp(dut_yjp115[31:0]);
gol_re_yjp116 = ieee754_to_fp(gol_yjp116[63:32]);
dut_re_yjp116 = ieee754_to_fp(dut_yjp116[63:32]);
gol_im_yjp116 = ieee754_to_fp(gol_yjp116[31:0]);
dut_im_yjp116 = ieee754_to_fp(dut_yjp116[31:0]);
gol_re_yjp117 = ieee754_to_fp(gol_yjp117[63:32]);
dut_re_yjp117 = ieee754_to_fp(dut_yjp117[63:32]);
gol_im_yjp117 = ieee754_to_fp(gol_yjp117[31:0]);
dut_im_yjp117 = ieee754_to_fp(dut_yjp117[31:0]);
gol_re_yjp118 = ieee754_to_fp(gol_yjp118[63:32]);
dut_re_yjp118 = ieee754_to_fp(dut_yjp118[63:32]);
gol_im_yjp118 = ieee754_to_fp(gol_yjp118[31:0]);
dut_im_yjp118 = ieee754_to_fp(dut_yjp118[31:0]);
gol_re_yjp119 = ieee754_to_fp(gol_yjp119[63:32]);
dut_re_yjp119 = ieee754_to_fp(dut_yjp119[63:32]);
gol_im_yjp119 = ieee754_to_fp(gol_yjp119[31:0]);
dut_im_yjp119 = ieee754_to_fp(dut_yjp119[31:0]);
gol_re_yjp120 = ieee754_to_fp(gol_yjp120[63:32]);
dut_re_yjp120 = ieee754_to_fp(dut_yjp120[63:32]);
gol_im_yjp120 = ieee754_to_fp(gol_yjp120[31:0]);
dut_im_yjp120 = ieee754_to_fp(dut_yjp120[31:0]);
gol_re_yjp121 = ieee754_to_fp(gol_yjp121[63:32]);
dut_re_yjp121 = ieee754_to_fp(dut_yjp121[63:32]);
gol_im_yjp121 = ieee754_to_fp(gol_yjp121[31:0]);
dut_im_yjp121 = ieee754_to_fp(dut_yjp121[31:0]);
gol_re_yjp122 = ieee754_to_fp(gol_yjp122[63:32]);
dut_re_yjp122 = ieee754_to_fp(dut_yjp122[63:32]);
gol_im_yjp122 = ieee754_to_fp(gol_yjp122[31:0]);
dut_im_yjp122 = ieee754_to_fp(dut_yjp122[31:0]);
gol_re_yjp123 = ieee754_to_fp(gol_yjp123[63:32]);
dut_re_yjp123 = ieee754_to_fp(dut_yjp123[63:32]);
gol_im_yjp123 = ieee754_to_fp(gol_yjp123[31:0]);
dut_im_yjp123 = ieee754_to_fp(dut_yjp123[31:0]);
gol_re_yjp124 = ieee754_to_fp(gol_yjp124[63:32]);
dut_re_yjp124 = ieee754_to_fp(dut_yjp124[63:32]);
gol_im_yjp124 = ieee754_to_fp(gol_yjp124[31:0]);
dut_im_yjp124 = ieee754_to_fp(dut_yjp124[31:0]);
gol_re_yjp125 = ieee754_to_fp(gol_yjp125[63:32]);
dut_re_yjp125 = ieee754_to_fp(dut_yjp125[63:32]);
gol_im_yjp125 = ieee754_to_fp(gol_yjp125[31:0]);
dut_im_yjp125 = ieee754_to_fp(dut_yjp125[31:0]);
gol_re_yjp126 = ieee754_to_fp(gol_yjp126[63:32]);
dut_re_yjp126 = ieee754_to_fp(dut_yjp126[63:32]);
gol_im_yjp126 = ieee754_to_fp(gol_yjp126[31:0]);
dut_im_yjp126 = ieee754_to_fp(dut_yjp126[31:0]);
gol_re_yjp127 = ieee754_to_fp(gol_yjp127[63:32]);
dut_re_yjp127 = ieee754_to_fp(dut_yjp127[63:32]);
gol_im_yjp127 = ieee754_to_fp(gol_yjp127[31:0]);
dut_im_yjp127 = ieee754_to_fp(dut_yjp127[31:0]);
gol_re_yjp128 = ieee754_to_fp(gol_yjp128[63:32]);
dut_re_yjp128 = ieee754_to_fp(dut_yjp128[63:32]);
gol_im_yjp128 = ieee754_to_fp(gol_yjp128[31:0]);
dut_im_yjp128 = ieee754_to_fp(dut_yjp128[31:0]);
gol_re_yjp129 = ieee754_to_fp(gol_yjp129[63:32]);
dut_re_yjp129 = ieee754_to_fp(dut_yjp129[63:32]);
gol_im_yjp129 = ieee754_to_fp(gol_yjp129[31:0]);
dut_im_yjp129 = ieee754_to_fp(dut_yjp129[31:0]);
gol_re_yjp130 = ieee754_to_fp(gol_yjp130[63:32]);
dut_re_yjp130 = ieee754_to_fp(dut_yjp130[63:32]);
gol_im_yjp130 = ieee754_to_fp(gol_yjp130[31:0]);
dut_im_yjp130 = ieee754_to_fp(dut_yjp130[31:0]);
gol_re_yjp131 = ieee754_to_fp(gol_yjp131[63:32]);
dut_re_yjp131 = ieee754_to_fp(dut_yjp131[63:32]);
gol_im_yjp131 = ieee754_to_fp(gol_yjp131[31:0]);
dut_im_yjp131 = ieee754_to_fp(dut_yjp131[31:0]);
gol_re_yjp132 = ieee754_to_fp(gol_yjp132[63:32]);
dut_re_yjp132 = ieee754_to_fp(dut_yjp132[63:32]);
gol_im_yjp132 = ieee754_to_fp(gol_yjp132[31:0]);
dut_im_yjp132 = ieee754_to_fp(dut_yjp132[31:0]);
gol_re_yjp133 = ieee754_to_fp(gol_yjp133[63:32]);
dut_re_yjp133 = ieee754_to_fp(dut_yjp133[63:32]);
gol_im_yjp133 = ieee754_to_fp(gol_yjp133[31:0]);
dut_im_yjp133 = ieee754_to_fp(dut_yjp133[31:0]);
gol_re_yjp134 = ieee754_to_fp(gol_yjp134[63:32]);
dut_re_yjp134 = ieee754_to_fp(dut_yjp134[63:32]);
gol_im_yjp134 = ieee754_to_fp(gol_yjp134[31:0]);
dut_im_yjp134 = ieee754_to_fp(dut_yjp134[31:0]);
gol_re_yjp135 = ieee754_to_fp(gol_yjp135[63:32]);
dut_re_yjp135 = ieee754_to_fp(dut_yjp135[63:32]);
gol_im_yjp135 = ieee754_to_fp(gol_yjp135[31:0]);
dut_im_yjp135 = ieee754_to_fp(dut_yjp135[31:0]);
gol_re_yjp136 = ieee754_to_fp(gol_yjp136[63:32]);
dut_re_yjp136 = ieee754_to_fp(dut_yjp136[63:32]);
gol_im_yjp136 = ieee754_to_fp(gol_yjp136[31:0]);
dut_im_yjp136 = ieee754_to_fp(dut_yjp136[31:0]);
gol_re_yjp137 = ieee754_to_fp(gol_yjp137[63:32]);
dut_re_yjp137 = ieee754_to_fp(dut_yjp137[63:32]);
gol_im_yjp137 = ieee754_to_fp(gol_yjp137[31:0]);
dut_im_yjp137 = ieee754_to_fp(dut_yjp137[31:0]);
gol_re_yjp138 = ieee754_to_fp(gol_yjp138[63:32]);
dut_re_yjp138 = ieee754_to_fp(dut_yjp138[63:32]);
gol_im_yjp138 = ieee754_to_fp(gol_yjp138[31:0]);
dut_im_yjp138 = ieee754_to_fp(dut_yjp138[31:0]);
gol_re_yjp139 = ieee754_to_fp(gol_yjp139[63:32]);
dut_re_yjp139 = ieee754_to_fp(dut_yjp139[63:32]);
gol_im_yjp139 = ieee754_to_fp(gol_yjp139[31:0]);
dut_im_yjp139 = ieee754_to_fp(dut_yjp139[31:0]);
gol_re_yjp140 = ieee754_to_fp(gol_yjp140[63:32]);
dut_re_yjp140 = ieee754_to_fp(dut_yjp140[63:32]);
gol_im_yjp140 = ieee754_to_fp(gol_yjp140[31:0]);
dut_im_yjp140 = ieee754_to_fp(dut_yjp140[31:0]);
gol_re_yjp141 = ieee754_to_fp(gol_yjp141[63:32]);
dut_re_yjp141 = ieee754_to_fp(dut_yjp141[63:32]);
gol_im_yjp141 = ieee754_to_fp(gol_yjp141[31:0]);
dut_im_yjp141 = ieee754_to_fp(dut_yjp141[31:0]);
gol_re_yjp142 = ieee754_to_fp(gol_yjp142[63:32]);
dut_re_yjp142 = ieee754_to_fp(dut_yjp142[63:32]);
gol_im_yjp142 = ieee754_to_fp(gol_yjp142[31:0]);
dut_im_yjp142 = ieee754_to_fp(dut_yjp142[31:0]);
gol_re_yjp143 = ieee754_to_fp(gol_yjp143[63:32]);
dut_re_yjp143 = ieee754_to_fp(dut_yjp143[63:32]);
gol_im_yjp143 = ieee754_to_fp(gol_yjp143[31:0]);
dut_im_yjp143 = ieee754_to_fp(dut_yjp143[31:0]);
gol_re_yjp144 = ieee754_to_fp(gol_yjp144[63:32]);
dut_re_yjp144 = ieee754_to_fp(dut_yjp144[63:32]);
gol_im_yjp144 = ieee754_to_fp(gol_yjp144[31:0]);
dut_im_yjp144 = ieee754_to_fp(dut_yjp144[31:0]);
gol_re_yjp145 = ieee754_to_fp(gol_yjp145[63:32]);
dut_re_yjp145 = ieee754_to_fp(dut_yjp145[63:32]);
gol_im_yjp145 = ieee754_to_fp(gol_yjp145[31:0]);
dut_im_yjp145 = ieee754_to_fp(dut_yjp145[31:0]);
gol_re_yjp146 = ieee754_to_fp(gol_yjp146[63:32]);
dut_re_yjp146 = ieee754_to_fp(dut_yjp146[63:32]);
gol_im_yjp146 = ieee754_to_fp(gol_yjp146[31:0]);
dut_im_yjp146 = ieee754_to_fp(dut_yjp146[31:0]);
gol_re_yjp147 = ieee754_to_fp(gol_yjp147[63:32]);
dut_re_yjp147 = ieee754_to_fp(dut_yjp147[63:32]);
gol_im_yjp147 = ieee754_to_fp(gol_yjp147[31:0]);
dut_im_yjp147 = ieee754_to_fp(dut_yjp147[31:0]);
gol_re_yjp148 = ieee754_to_fp(gol_yjp148[63:32]);
dut_re_yjp148 = ieee754_to_fp(dut_yjp148[63:32]);
gol_im_yjp148 = ieee754_to_fp(gol_yjp148[31:0]);
dut_im_yjp148 = ieee754_to_fp(dut_yjp148[31:0]);
gol_re_yjp149 = ieee754_to_fp(gol_yjp149[63:32]);
dut_re_yjp149 = ieee754_to_fp(dut_yjp149[63:32]);
gol_im_yjp149 = ieee754_to_fp(gol_yjp149[31:0]);
dut_im_yjp149 = ieee754_to_fp(dut_yjp149[31:0]);
gol_re_yjp150 = ieee754_to_fp(gol_yjp150[63:32]);
dut_re_yjp150 = ieee754_to_fp(dut_yjp150[63:32]);
gol_im_yjp150 = ieee754_to_fp(gol_yjp150[31:0]);
dut_im_yjp150 = ieee754_to_fp(dut_yjp150[31:0]);
gol_re_yjp151 = ieee754_to_fp(gol_yjp151[63:32]);
dut_re_yjp151 = ieee754_to_fp(dut_yjp151[63:32]);
gol_im_yjp151 = ieee754_to_fp(gol_yjp151[31:0]);
dut_im_yjp151 = ieee754_to_fp(dut_yjp151[31:0]);
gol_re_yjp152 = ieee754_to_fp(gol_yjp152[63:32]);
dut_re_yjp152 = ieee754_to_fp(dut_yjp152[63:32]);
gol_im_yjp152 = ieee754_to_fp(gol_yjp152[31:0]);
dut_im_yjp152 = ieee754_to_fp(dut_yjp152[31:0]);
gol_re_yjp153 = ieee754_to_fp(gol_yjp153[63:32]);
dut_re_yjp153 = ieee754_to_fp(dut_yjp153[63:32]);
gol_im_yjp153 = ieee754_to_fp(gol_yjp153[31:0]);
dut_im_yjp153 = ieee754_to_fp(dut_yjp153[31:0]);
gol_re_yjp154 = ieee754_to_fp(gol_yjp154[63:32]);
dut_re_yjp154 = ieee754_to_fp(dut_yjp154[63:32]);
gol_im_yjp154 = ieee754_to_fp(gol_yjp154[31:0]);
dut_im_yjp154 = ieee754_to_fp(dut_yjp154[31:0]);
gol_re_yjp155 = ieee754_to_fp(gol_yjp155[63:32]);
dut_re_yjp155 = ieee754_to_fp(dut_yjp155[63:32]);
gol_im_yjp155 = ieee754_to_fp(gol_yjp155[31:0]);
dut_im_yjp155 = ieee754_to_fp(dut_yjp155[31:0]);
gol_re_yjp156 = ieee754_to_fp(gol_yjp156[63:32]);
dut_re_yjp156 = ieee754_to_fp(dut_yjp156[63:32]);
gol_im_yjp156 = ieee754_to_fp(gol_yjp156[31:0]);
dut_im_yjp156 = ieee754_to_fp(dut_yjp156[31:0]);
gol_re_yjp157 = ieee754_to_fp(gol_yjp157[63:32]);
dut_re_yjp157 = ieee754_to_fp(dut_yjp157[63:32]);
gol_im_yjp157 = ieee754_to_fp(gol_yjp157[31:0]);
dut_im_yjp157 = ieee754_to_fp(dut_yjp157[31:0]);
gol_re_yjp158 = ieee754_to_fp(gol_yjp158[63:32]);
dut_re_yjp158 = ieee754_to_fp(dut_yjp158[63:32]);
gol_im_yjp158 = ieee754_to_fp(gol_yjp158[31:0]);
dut_im_yjp158 = ieee754_to_fp(dut_yjp158[31:0]);
gol_re_yjp159 = ieee754_to_fp(gol_yjp159[63:32]);
dut_re_yjp159 = ieee754_to_fp(dut_yjp159[63:32]);
gol_im_yjp159 = ieee754_to_fp(gol_yjp159[31:0]);
dut_im_yjp159 = ieee754_to_fp(dut_yjp159[31:0]);
gol_re_yjp160 = ieee754_to_fp(gol_yjp160[63:32]);
dut_re_yjp160 = ieee754_to_fp(dut_yjp160[63:32]);
gol_im_yjp160 = ieee754_to_fp(gol_yjp160[31:0]);
dut_im_yjp160 = ieee754_to_fp(dut_yjp160[31:0]);
gol_re_yjp161 = ieee754_to_fp(gol_yjp161[63:32]);
dut_re_yjp161 = ieee754_to_fp(dut_yjp161[63:32]);
gol_im_yjp161 = ieee754_to_fp(gol_yjp161[31:0]);
dut_im_yjp161 = ieee754_to_fp(dut_yjp161[31:0]);
gol_re_yjp162 = ieee754_to_fp(gol_yjp162[63:32]);
dut_re_yjp162 = ieee754_to_fp(dut_yjp162[63:32]);
gol_im_yjp162 = ieee754_to_fp(gol_yjp162[31:0]);
dut_im_yjp162 = ieee754_to_fp(dut_yjp162[31:0]);
gol_re_yjp163 = ieee754_to_fp(gol_yjp163[63:32]);
dut_re_yjp163 = ieee754_to_fp(dut_yjp163[63:32]);
gol_im_yjp163 = ieee754_to_fp(gol_yjp163[31:0]);
dut_im_yjp163 = ieee754_to_fp(dut_yjp163[31:0]);
gol_re_yjp164 = ieee754_to_fp(gol_yjp164[63:32]);
dut_re_yjp164 = ieee754_to_fp(dut_yjp164[63:32]);
gol_im_yjp164 = ieee754_to_fp(gol_yjp164[31:0]);
dut_im_yjp164 = ieee754_to_fp(dut_yjp164[31:0]);
gol_re_yjp165 = ieee754_to_fp(gol_yjp165[63:32]);
dut_re_yjp165 = ieee754_to_fp(dut_yjp165[63:32]);
gol_im_yjp165 = ieee754_to_fp(gol_yjp165[31:0]);
dut_im_yjp165 = ieee754_to_fp(dut_yjp165[31:0]);
gol_re_yjp166 = ieee754_to_fp(gol_yjp166[63:32]);
dut_re_yjp166 = ieee754_to_fp(dut_yjp166[63:32]);
gol_im_yjp166 = ieee754_to_fp(gol_yjp166[31:0]);
dut_im_yjp166 = ieee754_to_fp(dut_yjp166[31:0]);
gol_re_yjp167 = ieee754_to_fp(gol_yjp167[63:32]);
dut_re_yjp167 = ieee754_to_fp(dut_yjp167[63:32]);
gol_im_yjp167 = ieee754_to_fp(gol_yjp167[31:0]);
dut_im_yjp167 = ieee754_to_fp(dut_yjp167[31:0]);
gol_re_yjp168 = ieee754_to_fp(gol_yjp168[63:32]);
dut_re_yjp168 = ieee754_to_fp(dut_yjp168[63:32]);
gol_im_yjp168 = ieee754_to_fp(gol_yjp168[31:0]);
dut_im_yjp168 = ieee754_to_fp(dut_yjp168[31:0]);
gol_re_yjp169 = ieee754_to_fp(gol_yjp169[63:32]);
dut_re_yjp169 = ieee754_to_fp(dut_yjp169[63:32]);
gol_im_yjp169 = ieee754_to_fp(gol_yjp169[31:0]);
dut_im_yjp169 = ieee754_to_fp(dut_yjp169[31:0]);
gol_re_yjp170 = ieee754_to_fp(gol_yjp170[63:32]);
dut_re_yjp170 = ieee754_to_fp(dut_yjp170[63:32]);
gol_im_yjp170 = ieee754_to_fp(gol_yjp170[31:0]);
dut_im_yjp170 = ieee754_to_fp(dut_yjp170[31:0]);
gol_re_yjp171 = ieee754_to_fp(gol_yjp171[63:32]);
dut_re_yjp171 = ieee754_to_fp(dut_yjp171[63:32]);
gol_im_yjp171 = ieee754_to_fp(gol_yjp171[31:0]);
dut_im_yjp171 = ieee754_to_fp(dut_yjp171[31:0]);
gol_re_yjp172 = ieee754_to_fp(gol_yjp172[63:32]);
dut_re_yjp172 = ieee754_to_fp(dut_yjp172[63:32]);
gol_im_yjp172 = ieee754_to_fp(gol_yjp172[31:0]);
dut_im_yjp172 = ieee754_to_fp(dut_yjp172[31:0]);
gol_re_yjp173 = ieee754_to_fp(gol_yjp173[63:32]);
dut_re_yjp173 = ieee754_to_fp(dut_yjp173[63:32]);
gol_im_yjp173 = ieee754_to_fp(gol_yjp173[31:0]);
dut_im_yjp173 = ieee754_to_fp(dut_yjp173[31:0]);
gol_re_yjp174 = ieee754_to_fp(gol_yjp174[63:32]);
dut_re_yjp174 = ieee754_to_fp(dut_yjp174[63:32]);
gol_im_yjp174 = ieee754_to_fp(gol_yjp174[31:0]);
dut_im_yjp174 = ieee754_to_fp(dut_yjp174[31:0]);
gol_re_yjp175 = ieee754_to_fp(gol_yjp175[63:32]);
dut_re_yjp175 = ieee754_to_fp(dut_yjp175[63:32]);
gol_im_yjp175 = ieee754_to_fp(gol_yjp175[31:0]);
dut_im_yjp175 = ieee754_to_fp(dut_yjp175[31:0]);
gol_re_yjp176 = ieee754_to_fp(gol_yjp176[63:32]);
dut_re_yjp176 = ieee754_to_fp(dut_yjp176[63:32]);
gol_im_yjp176 = ieee754_to_fp(gol_yjp176[31:0]);
dut_im_yjp176 = ieee754_to_fp(dut_yjp176[31:0]);
gol_re_yjp177 = ieee754_to_fp(gol_yjp177[63:32]);
dut_re_yjp177 = ieee754_to_fp(dut_yjp177[63:32]);
gol_im_yjp177 = ieee754_to_fp(gol_yjp177[31:0]);
dut_im_yjp177 = ieee754_to_fp(dut_yjp177[31:0]);
gol_re_yjp178 = ieee754_to_fp(gol_yjp178[63:32]);
dut_re_yjp178 = ieee754_to_fp(dut_yjp178[63:32]);
gol_im_yjp178 = ieee754_to_fp(gol_yjp178[31:0]);
dut_im_yjp178 = ieee754_to_fp(dut_yjp178[31:0]);
gol_re_yjp179 = ieee754_to_fp(gol_yjp179[63:32]);
dut_re_yjp179 = ieee754_to_fp(dut_yjp179[63:32]);
gol_im_yjp179 = ieee754_to_fp(gol_yjp179[31:0]);
dut_im_yjp179 = ieee754_to_fp(dut_yjp179[31:0]);
gol_re_yjp180 = ieee754_to_fp(gol_yjp180[63:32]);
dut_re_yjp180 = ieee754_to_fp(dut_yjp180[63:32]);
gol_im_yjp180 = ieee754_to_fp(gol_yjp180[31:0]);
dut_im_yjp180 = ieee754_to_fp(dut_yjp180[31:0]);
gol_re_yjp181 = ieee754_to_fp(gol_yjp181[63:32]);
dut_re_yjp181 = ieee754_to_fp(dut_yjp181[63:32]);
gol_im_yjp181 = ieee754_to_fp(gol_yjp181[31:0]);
dut_im_yjp181 = ieee754_to_fp(dut_yjp181[31:0]);
gol_re_yjp182 = ieee754_to_fp(gol_yjp182[63:32]);
dut_re_yjp182 = ieee754_to_fp(dut_yjp182[63:32]);
gol_im_yjp182 = ieee754_to_fp(gol_yjp182[31:0]);
dut_im_yjp182 = ieee754_to_fp(dut_yjp182[31:0]);
gol_re_yjp183 = ieee754_to_fp(gol_yjp183[63:32]);
dut_re_yjp183 = ieee754_to_fp(dut_yjp183[63:32]);
gol_im_yjp183 = ieee754_to_fp(gol_yjp183[31:0]);
dut_im_yjp183 = ieee754_to_fp(dut_yjp183[31:0]);
gol_re_yjp184 = ieee754_to_fp(gol_yjp184[63:32]);
dut_re_yjp184 = ieee754_to_fp(dut_yjp184[63:32]);
gol_im_yjp184 = ieee754_to_fp(gol_yjp184[31:0]);
dut_im_yjp184 = ieee754_to_fp(dut_yjp184[31:0]);
gol_re_yjp185 = ieee754_to_fp(gol_yjp185[63:32]);
dut_re_yjp185 = ieee754_to_fp(dut_yjp185[63:32]);
gol_im_yjp185 = ieee754_to_fp(gol_yjp185[31:0]);
dut_im_yjp185 = ieee754_to_fp(dut_yjp185[31:0]);
gol_re_yjp186 = ieee754_to_fp(gol_yjp186[63:32]);
dut_re_yjp186 = ieee754_to_fp(dut_yjp186[63:32]);
gol_im_yjp186 = ieee754_to_fp(gol_yjp186[31:0]);
dut_im_yjp186 = ieee754_to_fp(dut_yjp186[31:0]);
gol_re_yjp187 = ieee754_to_fp(gol_yjp187[63:32]);
dut_re_yjp187 = ieee754_to_fp(dut_yjp187[63:32]);
gol_im_yjp187 = ieee754_to_fp(gol_yjp187[31:0]);
dut_im_yjp187 = ieee754_to_fp(dut_yjp187[31:0]);
gol_re_yjp188 = ieee754_to_fp(gol_yjp188[63:32]);
dut_re_yjp188 = ieee754_to_fp(dut_yjp188[63:32]);
gol_im_yjp188 = ieee754_to_fp(gol_yjp188[31:0]);
dut_im_yjp188 = ieee754_to_fp(dut_yjp188[31:0]);
gol_re_yjp189 = ieee754_to_fp(gol_yjp189[63:32]);
dut_re_yjp189 = ieee754_to_fp(dut_yjp189[63:32]);
gol_im_yjp189 = ieee754_to_fp(gol_yjp189[31:0]);
dut_im_yjp189 = ieee754_to_fp(dut_yjp189[31:0]);
gol_re_yjp190 = ieee754_to_fp(gol_yjp190[63:32]);
dut_re_yjp190 = ieee754_to_fp(dut_yjp190[63:32]);
gol_im_yjp190 = ieee754_to_fp(gol_yjp190[31:0]);
dut_im_yjp190 = ieee754_to_fp(dut_yjp190[31:0]);
gol_re_yjp191 = ieee754_to_fp(gol_yjp191[63:32]);
dut_re_yjp191 = ieee754_to_fp(dut_yjp191[63:32]);
gol_im_yjp191 = ieee754_to_fp(gol_yjp191[31:0]);
dut_im_yjp191 = ieee754_to_fp(dut_yjp191[31:0]);
gol_re_yjp192 = ieee754_to_fp(gol_yjp192[63:32]);
dut_re_yjp192 = ieee754_to_fp(dut_yjp192[63:32]);
gol_im_yjp192 = ieee754_to_fp(gol_yjp192[31:0]);
dut_im_yjp192 = ieee754_to_fp(dut_yjp192[31:0]);
gol_re_yjp193 = ieee754_to_fp(gol_yjp193[63:32]);
dut_re_yjp193 = ieee754_to_fp(dut_yjp193[63:32]);
gol_im_yjp193 = ieee754_to_fp(gol_yjp193[31:0]);
dut_im_yjp193 = ieee754_to_fp(dut_yjp193[31:0]);
gol_re_yjp194 = ieee754_to_fp(gol_yjp194[63:32]);
dut_re_yjp194 = ieee754_to_fp(dut_yjp194[63:32]);
gol_im_yjp194 = ieee754_to_fp(gol_yjp194[31:0]);
dut_im_yjp194 = ieee754_to_fp(dut_yjp194[31:0]);
gol_re_yjp195 = ieee754_to_fp(gol_yjp195[63:32]);
dut_re_yjp195 = ieee754_to_fp(dut_yjp195[63:32]);
gol_im_yjp195 = ieee754_to_fp(gol_yjp195[31:0]);
dut_im_yjp195 = ieee754_to_fp(dut_yjp195[31:0]);
gol_re_yjp196 = ieee754_to_fp(gol_yjp196[63:32]);
dut_re_yjp196 = ieee754_to_fp(dut_yjp196[63:32]);
gol_im_yjp196 = ieee754_to_fp(gol_yjp196[31:0]);
dut_im_yjp196 = ieee754_to_fp(dut_yjp196[31:0]);
gol_re_yjp197 = ieee754_to_fp(gol_yjp197[63:32]);
dut_re_yjp197 = ieee754_to_fp(dut_yjp197[63:32]);
gol_im_yjp197 = ieee754_to_fp(gol_yjp197[31:0]);
dut_im_yjp197 = ieee754_to_fp(dut_yjp197[31:0]);
gol_re_yjp198 = ieee754_to_fp(gol_yjp198[63:32]);
dut_re_yjp198 = ieee754_to_fp(dut_yjp198[63:32]);
gol_im_yjp198 = ieee754_to_fp(gol_yjp198[31:0]);
dut_im_yjp198 = ieee754_to_fp(dut_yjp198[31:0]);
gol_re_yjp199 = ieee754_to_fp(gol_yjp199[63:32]);
dut_re_yjp199 = ieee754_to_fp(dut_yjp199[63:32]);
gol_im_yjp199 = ieee754_to_fp(gol_yjp199[31:0]);
dut_im_yjp199 = ieee754_to_fp(dut_yjp199[31:0]);
gol_re_yjp200 = ieee754_to_fp(gol_yjp200[63:32]);
dut_re_yjp200 = ieee754_to_fp(dut_yjp200[63:32]);
gol_im_yjp200 = ieee754_to_fp(gol_yjp200[31:0]);
dut_im_yjp200 = ieee754_to_fp(dut_yjp200[31:0]);
gol_re_yjp201 = ieee754_to_fp(gol_yjp201[63:32]);
dut_re_yjp201 = ieee754_to_fp(dut_yjp201[63:32]);
gol_im_yjp201 = ieee754_to_fp(gol_yjp201[31:0]);
dut_im_yjp201 = ieee754_to_fp(dut_yjp201[31:0]);
gol_re_yjp202 = ieee754_to_fp(gol_yjp202[63:32]);
dut_re_yjp202 = ieee754_to_fp(dut_yjp202[63:32]);
gol_im_yjp202 = ieee754_to_fp(gol_yjp202[31:0]);
dut_im_yjp202 = ieee754_to_fp(dut_yjp202[31:0]);
gol_re_yjp203 = ieee754_to_fp(gol_yjp203[63:32]);
dut_re_yjp203 = ieee754_to_fp(dut_yjp203[63:32]);
gol_im_yjp203 = ieee754_to_fp(gol_yjp203[31:0]);
dut_im_yjp203 = ieee754_to_fp(dut_yjp203[31:0]);
gol_re_yjp204 = ieee754_to_fp(gol_yjp204[63:32]);
dut_re_yjp204 = ieee754_to_fp(dut_yjp204[63:32]);
gol_im_yjp204 = ieee754_to_fp(gol_yjp204[31:0]);
dut_im_yjp204 = ieee754_to_fp(dut_yjp204[31:0]);
gol_re_yjp205 = ieee754_to_fp(gol_yjp205[63:32]);
dut_re_yjp205 = ieee754_to_fp(dut_yjp205[63:32]);
gol_im_yjp205 = ieee754_to_fp(gol_yjp205[31:0]);
dut_im_yjp205 = ieee754_to_fp(dut_yjp205[31:0]);
gol_re_yjp206 = ieee754_to_fp(gol_yjp206[63:32]);
dut_re_yjp206 = ieee754_to_fp(dut_yjp206[63:32]);
gol_im_yjp206 = ieee754_to_fp(gol_yjp206[31:0]);
dut_im_yjp206 = ieee754_to_fp(dut_yjp206[31:0]);
gol_re_yjp207 = ieee754_to_fp(gol_yjp207[63:32]);
dut_re_yjp207 = ieee754_to_fp(dut_yjp207[63:32]);
gol_im_yjp207 = ieee754_to_fp(gol_yjp207[31:0]);
dut_im_yjp207 = ieee754_to_fp(dut_yjp207[31:0]);
gol_re_yjp208 = ieee754_to_fp(gol_yjp208[63:32]);
dut_re_yjp208 = ieee754_to_fp(dut_yjp208[63:32]);
gol_im_yjp208 = ieee754_to_fp(gol_yjp208[31:0]);
dut_im_yjp208 = ieee754_to_fp(dut_yjp208[31:0]);
gol_re_yjp209 = ieee754_to_fp(gol_yjp209[63:32]);
dut_re_yjp209 = ieee754_to_fp(dut_yjp209[63:32]);
gol_im_yjp209 = ieee754_to_fp(gol_yjp209[31:0]);
dut_im_yjp209 = ieee754_to_fp(dut_yjp209[31:0]);
gol_re_yjp210 = ieee754_to_fp(gol_yjp210[63:32]);
dut_re_yjp210 = ieee754_to_fp(dut_yjp210[63:32]);
gol_im_yjp210 = ieee754_to_fp(gol_yjp210[31:0]);
dut_im_yjp210 = ieee754_to_fp(dut_yjp210[31:0]);
gol_re_yjp211 = ieee754_to_fp(gol_yjp211[63:32]);
dut_re_yjp211 = ieee754_to_fp(dut_yjp211[63:32]);
gol_im_yjp211 = ieee754_to_fp(gol_yjp211[31:0]);
dut_im_yjp211 = ieee754_to_fp(dut_yjp211[31:0]);
gol_re_yjp212 = ieee754_to_fp(gol_yjp212[63:32]);
dut_re_yjp212 = ieee754_to_fp(dut_yjp212[63:32]);
gol_im_yjp212 = ieee754_to_fp(gol_yjp212[31:0]);
dut_im_yjp212 = ieee754_to_fp(dut_yjp212[31:0]);
gol_re_yjp213 = ieee754_to_fp(gol_yjp213[63:32]);
dut_re_yjp213 = ieee754_to_fp(dut_yjp213[63:32]);
gol_im_yjp213 = ieee754_to_fp(gol_yjp213[31:0]);
dut_im_yjp213 = ieee754_to_fp(dut_yjp213[31:0]);
gol_re_yjp214 = ieee754_to_fp(gol_yjp214[63:32]);
dut_re_yjp214 = ieee754_to_fp(dut_yjp214[63:32]);
gol_im_yjp214 = ieee754_to_fp(gol_yjp214[31:0]);
dut_im_yjp214 = ieee754_to_fp(dut_yjp214[31:0]);
gol_re_yjp215 = ieee754_to_fp(gol_yjp215[63:32]);
dut_re_yjp215 = ieee754_to_fp(dut_yjp215[63:32]);
gol_im_yjp215 = ieee754_to_fp(gol_yjp215[31:0]);
dut_im_yjp215 = ieee754_to_fp(dut_yjp215[31:0]);
gol_re_yjp216 = ieee754_to_fp(gol_yjp216[63:32]);
dut_re_yjp216 = ieee754_to_fp(dut_yjp216[63:32]);
gol_im_yjp216 = ieee754_to_fp(gol_yjp216[31:0]);
dut_im_yjp216 = ieee754_to_fp(dut_yjp216[31:0]);
gol_re_yjp217 = ieee754_to_fp(gol_yjp217[63:32]);
dut_re_yjp217 = ieee754_to_fp(dut_yjp217[63:32]);
gol_im_yjp217 = ieee754_to_fp(gol_yjp217[31:0]);
dut_im_yjp217 = ieee754_to_fp(dut_yjp217[31:0]);
gol_re_yjp218 = ieee754_to_fp(gol_yjp218[63:32]);
dut_re_yjp218 = ieee754_to_fp(dut_yjp218[63:32]);
gol_im_yjp218 = ieee754_to_fp(gol_yjp218[31:0]);
dut_im_yjp218 = ieee754_to_fp(dut_yjp218[31:0]);
gol_re_yjp219 = ieee754_to_fp(gol_yjp219[63:32]);
dut_re_yjp219 = ieee754_to_fp(dut_yjp219[63:32]);
gol_im_yjp219 = ieee754_to_fp(gol_yjp219[31:0]);
dut_im_yjp219 = ieee754_to_fp(dut_yjp219[31:0]);
gol_re_yjp220 = ieee754_to_fp(gol_yjp220[63:32]);
dut_re_yjp220 = ieee754_to_fp(dut_yjp220[63:32]);
gol_im_yjp220 = ieee754_to_fp(gol_yjp220[31:0]);
dut_im_yjp220 = ieee754_to_fp(dut_yjp220[31:0]);
gol_re_yjp221 = ieee754_to_fp(gol_yjp221[63:32]);
dut_re_yjp221 = ieee754_to_fp(dut_yjp221[63:32]);
gol_im_yjp221 = ieee754_to_fp(gol_yjp221[31:0]);
dut_im_yjp221 = ieee754_to_fp(dut_yjp221[31:0]);
gol_re_yjp222 = ieee754_to_fp(gol_yjp222[63:32]);
dut_re_yjp222 = ieee754_to_fp(dut_yjp222[63:32]);
gol_im_yjp222 = ieee754_to_fp(gol_yjp222[31:0]);
dut_im_yjp222 = ieee754_to_fp(dut_yjp222[31:0]);
gol_re_yjp223 = ieee754_to_fp(gol_yjp223[63:32]);
dut_re_yjp223 = ieee754_to_fp(dut_yjp223[63:32]);
gol_im_yjp223 = ieee754_to_fp(gol_yjp223[31:0]);
dut_im_yjp223 = ieee754_to_fp(dut_yjp223[31:0]);
gol_re_yjp224 = ieee754_to_fp(gol_yjp224[63:32]);
dut_re_yjp224 = ieee754_to_fp(dut_yjp224[63:32]);
gol_im_yjp224 = ieee754_to_fp(gol_yjp224[31:0]);
dut_im_yjp224 = ieee754_to_fp(dut_yjp224[31:0]);
gol_re_yjp225 = ieee754_to_fp(gol_yjp225[63:32]);
dut_re_yjp225 = ieee754_to_fp(dut_yjp225[63:32]);
gol_im_yjp225 = ieee754_to_fp(gol_yjp225[31:0]);
dut_im_yjp225 = ieee754_to_fp(dut_yjp225[31:0]);
gol_re_yjp226 = ieee754_to_fp(gol_yjp226[63:32]);
dut_re_yjp226 = ieee754_to_fp(dut_yjp226[63:32]);
gol_im_yjp226 = ieee754_to_fp(gol_yjp226[31:0]);
dut_im_yjp226 = ieee754_to_fp(dut_yjp226[31:0]);
gol_re_yjp227 = ieee754_to_fp(gol_yjp227[63:32]);
dut_re_yjp227 = ieee754_to_fp(dut_yjp227[63:32]);
gol_im_yjp227 = ieee754_to_fp(gol_yjp227[31:0]);
dut_im_yjp227 = ieee754_to_fp(dut_yjp227[31:0]);
gol_re_yjp228 = ieee754_to_fp(gol_yjp228[63:32]);
dut_re_yjp228 = ieee754_to_fp(dut_yjp228[63:32]);
gol_im_yjp228 = ieee754_to_fp(gol_yjp228[31:0]);
dut_im_yjp228 = ieee754_to_fp(dut_yjp228[31:0]);
gol_re_yjp229 = ieee754_to_fp(gol_yjp229[63:32]);
dut_re_yjp229 = ieee754_to_fp(dut_yjp229[63:32]);
gol_im_yjp229 = ieee754_to_fp(gol_yjp229[31:0]);
dut_im_yjp229 = ieee754_to_fp(dut_yjp229[31:0]);
gol_re_yjp230 = ieee754_to_fp(gol_yjp230[63:32]);
dut_re_yjp230 = ieee754_to_fp(dut_yjp230[63:32]);
gol_im_yjp230 = ieee754_to_fp(gol_yjp230[31:0]);
dut_im_yjp230 = ieee754_to_fp(dut_yjp230[31:0]);
gol_re_yjp231 = ieee754_to_fp(gol_yjp231[63:32]);
dut_re_yjp231 = ieee754_to_fp(dut_yjp231[63:32]);
gol_im_yjp231 = ieee754_to_fp(gol_yjp231[31:0]);
dut_im_yjp231 = ieee754_to_fp(dut_yjp231[31:0]);
gol_re_yjp232 = ieee754_to_fp(gol_yjp232[63:32]);
dut_re_yjp232 = ieee754_to_fp(dut_yjp232[63:32]);
gol_im_yjp232 = ieee754_to_fp(gol_yjp232[31:0]);
dut_im_yjp232 = ieee754_to_fp(dut_yjp232[31:0]);
gol_re_yjp233 = ieee754_to_fp(gol_yjp233[63:32]);
dut_re_yjp233 = ieee754_to_fp(dut_yjp233[63:32]);
gol_im_yjp233 = ieee754_to_fp(gol_yjp233[31:0]);
dut_im_yjp233 = ieee754_to_fp(dut_yjp233[31:0]);
gol_re_yjp234 = ieee754_to_fp(gol_yjp234[63:32]);
dut_re_yjp234 = ieee754_to_fp(dut_yjp234[63:32]);
gol_im_yjp234 = ieee754_to_fp(gol_yjp234[31:0]);
dut_im_yjp234 = ieee754_to_fp(dut_yjp234[31:0]);
gol_re_yjp235 = ieee754_to_fp(gol_yjp235[63:32]);
dut_re_yjp235 = ieee754_to_fp(dut_yjp235[63:32]);
gol_im_yjp235 = ieee754_to_fp(gol_yjp235[31:0]);
dut_im_yjp235 = ieee754_to_fp(dut_yjp235[31:0]);
gol_re_yjp236 = ieee754_to_fp(gol_yjp236[63:32]);
dut_re_yjp236 = ieee754_to_fp(dut_yjp236[63:32]);
gol_im_yjp236 = ieee754_to_fp(gol_yjp236[31:0]);
dut_im_yjp236 = ieee754_to_fp(dut_yjp236[31:0]);
gol_re_yjp237 = ieee754_to_fp(gol_yjp237[63:32]);
dut_re_yjp237 = ieee754_to_fp(dut_yjp237[63:32]);
gol_im_yjp237 = ieee754_to_fp(gol_yjp237[31:0]);
dut_im_yjp237 = ieee754_to_fp(dut_yjp237[31:0]);
gol_re_yjp238 = ieee754_to_fp(gol_yjp238[63:32]);
dut_re_yjp238 = ieee754_to_fp(dut_yjp238[63:32]);
gol_im_yjp238 = ieee754_to_fp(gol_yjp238[31:0]);
dut_im_yjp238 = ieee754_to_fp(dut_yjp238[31:0]);
gol_re_yjp239 = ieee754_to_fp(gol_yjp239[63:32]);
dut_re_yjp239 = ieee754_to_fp(dut_yjp239[63:32]);
gol_im_yjp239 = ieee754_to_fp(gol_yjp239[31:0]);
dut_im_yjp239 = ieee754_to_fp(dut_yjp239[31:0]);
gol_re_yjp240 = ieee754_to_fp(gol_yjp240[63:32]);
dut_re_yjp240 = ieee754_to_fp(dut_yjp240[63:32]);
gol_im_yjp240 = ieee754_to_fp(gol_yjp240[31:0]);
dut_im_yjp240 = ieee754_to_fp(dut_yjp240[31:0]);
gol_re_yjp241 = ieee754_to_fp(gol_yjp241[63:32]);
dut_re_yjp241 = ieee754_to_fp(dut_yjp241[63:32]);
gol_im_yjp241 = ieee754_to_fp(gol_yjp241[31:0]);
dut_im_yjp241 = ieee754_to_fp(dut_yjp241[31:0]);
gol_re_yjp242 = ieee754_to_fp(gol_yjp242[63:32]);
dut_re_yjp242 = ieee754_to_fp(dut_yjp242[63:32]);
gol_im_yjp242 = ieee754_to_fp(gol_yjp242[31:0]);
dut_im_yjp242 = ieee754_to_fp(dut_yjp242[31:0]);
gol_re_yjp243 = ieee754_to_fp(gol_yjp243[63:32]);
dut_re_yjp243 = ieee754_to_fp(dut_yjp243[63:32]);
gol_im_yjp243 = ieee754_to_fp(gol_yjp243[31:0]);
dut_im_yjp243 = ieee754_to_fp(dut_yjp243[31:0]);
gol_re_yjp244 = ieee754_to_fp(gol_yjp244[63:32]);
dut_re_yjp244 = ieee754_to_fp(dut_yjp244[63:32]);
gol_im_yjp244 = ieee754_to_fp(gol_yjp244[31:0]);
dut_im_yjp244 = ieee754_to_fp(dut_yjp244[31:0]);
gol_re_yjp245 = ieee754_to_fp(gol_yjp245[63:32]);
dut_re_yjp245 = ieee754_to_fp(dut_yjp245[63:32]);
gol_im_yjp245 = ieee754_to_fp(gol_yjp245[31:0]);
dut_im_yjp245 = ieee754_to_fp(dut_yjp245[31:0]);
gol_re_yjp246 = ieee754_to_fp(gol_yjp246[63:32]);
dut_re_yjp246 = ieee754_to_fp(dut_yjp246[63:32]);
gol_im_yjp246 = ieee754_to_fp(gol_yjp246[31:0]);
dut_im_yjp246 = ieee754_to_fp(dut_yjp246[31:0]);
gol_re_yjp247 = ieee754_to_fp(gol_yjp247[63:32]);
dut_re_yjp247 = ieee754_to_fp(dut_yjp247[63:32]);
gol_im_yjp247 = ieee754_to_fp(gol_yjp247[31:0]);
dut_im_yjp247 = ieee754_to_fp(dut_yjp247[31:0]);
gol_re_yjp248 = ieee754_to_fp(gol_yjp248[63:32]);
dut_re_yjp248 = ieee754_to_fp(dut_yjp248[63:32]);
gol_im_yjp248 = ieee754_to_fp(gol_yjp248[31:0]);
dut_im_yjp248 = ieee754_to_fp(dut_yjp248[31:0]);
gol_re_yjp249 = ieee754_to_fp(gol_yjp249[63:32]);
dut_re_yjp249 = ieee754_to_fp(dut_yjp249[63:32]);
gol_im_yjp249 = ieee754_to_fp(gol_yjp249[31:0]);
dut_im_yjp249 = ieee754_to_fp(dut_yjp249[31:0]);
gol_re_yjp250 = ieee754_to_fp(gol_yjp250[63:32]);
dut_re_yjp250 = ieee754_to_fp(dut_yjp250[63:32]);
gol_im_yjp250 = ieee754_to_fp(gol_yjp250[31:0]);
dut_im_yjp250 = ieee754_to_fp(dut_yjp250[31:0]);
gol_re_yjp251 = ieee754_to_fp(gol_yjp251[63:32]);
dut_re_yjp251 = ieee754_to_fp(dut_yjp251[63:32]);
gol_im_yjp251 = ieee754_to_fp(gol_yjp251[31:0]);
dut_im_yjp251 = ieee754_to_fp(dut_yjp251[31:0]);
gol_re_yjp252 = ieee754_to_fp(gol_yjp252[63:32]);
dut_re_yjp252 = ieee754_to_fp(dut_yjp252[63:32]);
gol_im_yjp252 = ieee754_to_fp(gol_yjp252[31:0]);
dut_im_yjp252 = ieee754_to_fp(dut_yjp252[31:0]);
gol_re_yjp253 = ieee754_to_fp(gol_yjp253[63:32]);
dut_re_yjp253 = ieee754_to_fp(dut_yjp253[63:32]);
gol_im_yjp253 = ieee754_to_fp(gol_yjp253[31:0]);
dut_im_yjp253 = ieee754_to_fp(dut_yjp253[31:0]);
gol_re_yjp254 = ieee754_to_fp(gol_yjp254[63:32]);
dut_re_yjp254 = ieee754_to_fp(dut_yjp254[63:32]);
gol_im_yjp254 = ieee754_to_fp(gol_yjp254[31:0]);
dut_im_yjp254 = ieee754_to_fp(dut_yjp254[31:0]);
gol_re_yjp255 = ieee754_to_fp(gol_yjp255[63:32]);
dut_re_yjp255 = ieee754_to_fp(dut_yjp255[63:32]);
gol_im_yjp255 = ieee754_to_fp(gol_yjp255[31:0]);
dut_im_yjp255 = ieee754_to_fp(dut_yjp255[31:0]);

if(gol_yjp0[63:32] == 0) begin err_re_per0 = (dut_re_yjp0 - gol_re_yjp0); end else begin err_re_per0 = ((dut_re_yjp0 - gol_re_yjp0) / gol_re_yjp0) * 100; end
if(gol_yjp0[31:0] == 0) begin err_im_per0 = (dut_im_yjp0 - gol_im_yjp0); end else begin err_im_per0 = ((dut_im_yjp0 - gol_im_yjp0) / gol_im_yjp0) * 100; end
if(gol_yjp1[63:32] == 0) begin err_re_per1 = (dut_re_yjp1 - gol_re_yjp1); end else begin err_re_per1 = ((dut_re_yjp1 - gol_re_yjp1) / gol_re_yjp1) * 100; end
if(gol_yjp1[31:0] == 0) begin err_im_per1 = (dut_im_yjp1 - gol_im_yjp1); end else begin err_im_per1 = ((dut_im_yjp1 - gol_im_yjp1) / gol_im_yjp1) * 100; end
if(gol_yjp2[63:32] == 0) begin err_re_per2 = (dut_re_yjp2 - gol_re_yjp2); end else begin err_re_per2 = ((dut_re_yjp2 - gol_re_yjp2) / gol_re_yjp2) * 100; end
if(gol_yjp2[31:0] == 0) begin err_im_per2 = (dut_im_yjp2 - gol_im_yjp2); end else begin err_im_per2 = ((dut_im_yjp2 - gol_im_yjp2) / gol_im_yjp2) * 100; end
if(gol_yjp3[63:32] == 0) begin err_re_per3 = (dut_re_yjp3 - gol_re_yjp3); end else begin err_re_per3 = ((dut_re_yjp3 - gol_re_yjp3) / gol_re_yjp3) * 100; end
if(gol_yjp3[31:0] == 0) begin err_im_per3 = (dut_im_yjp3 - gol_im_yjp3); end else begin err_im_per3 = ((dut_im_yjp3 - gol_im_yjp3) / gol_im_yjp3) * 100; end
if(gol_yjp4[63:32] == 0) begin err_re_per4 = (dut_re_yjp4 - gol_re_yjp4); end else begin err_re_per4 = ((dut_re_yjp4 - gol_re_yjp4) / gol_re_yjp4) * 100; end
if(gol_yjp4[31:0] == 0) begin err_im_per4 = (dut_im_yjp4 - gol_im_yjp4); end else begin err_im_per4 = ((dut_im_yjp4 - gol_im_yjp4) / gol_im_yjp4) * 100; end
if(gol_yjp5[63:32] == 0) begin err_re_per5 = (dut_re_yjp5 - gol_re_yjp5); end else begin err_re_per5 = ((dut_re_yjp5 - gol_re_yjp5) / gol_re_yjp5) * 100; end
if(gol_yjp5[31:0] == 0) begin err_im_per5 = (dut_im_yjp5 - gol_im_yjp5); end else begin err_im_per5 = ((dut_im_yjp5 - gol_im_yjp5) / gol_im_yjp5) * 100; end
if(gol_yjp6[63:32] == 0) begin err_re_per6 = (dut_re_yjp6 - gol_re_yjp6); end else begin err_re_per6 = ((dut_re_yjp6 - gol_re_yjp6) / gol_re_yjp6) * 100; end
if(gol_yjp6[31:0] == 0) begin err_im_per6 = (dut_im_yjp6 - gol_im_yjp6); end else begin err_im_per6 = ((dut_im_yjp6 - gol_im_yjp6) / gol_im_yjp6) * 100; end
if(gol_yjp7[63:32] == 0) begin err_re_per7 = (dut_re_yjp7 - gol_re_yjp7); end else begin err_re_per7 = ((dut_re_yjp7 - gol_re_yjp7) / gol_re_yjp7) * 100; end
if(gol_yjp7[31:0] == 0) begin err_im_per7 = (dut_im_yjp7 - gol_im_yjp7); end else begin err_im_per7 = ((dut_im_yjp7 - gol_im_yjp7) / gol_im_yjp7) * 100; end
if(gol_yjp8[63:32] == 0) begin err_re_per8 = (dut_re_yjp8 - gol_re_yjp8); end else begin err_re_per8 = ((dut_re_yjp8 - gol_re_yjp8) / gol_re_yjp8) * 100; end
if(gol_yjp8[31:0] == 0) begin err_im_per8 = (dut_im_yjp8 - gol_im_yjp8); end else begin err_im_per8 = ((dut_im_yjp8 - gol_im_yjp8) / gol_im_yjp8) * 100; end
if(gol_yjp9[63:32] == 0) begin err_re_per9 = (dut_re_yjp9 - gol_re_yjp9); end else begin err_re_per9 = ((dut_re_yjp9 - gol_re_yjp9) / gol_re_yjp9) * 100; end
if(gol_yjp9[31:0] == 0) begin err_im_per9 = (dut_im_yjp9 - gol_im_yjp9); end else begin err_im_per9 = ((dut_im_yjp9 - gol_im_yjp9) / gol_im_yjp9) * 100; end
if(gol_yjp10[63:32] == 0) begin err_re_per10 = (dut_re_yjp10 - gol_re_yjp10); end else begin err_re_per10 = ((dut_re_yjp10 - gol_re_yjp10) / gol_re_yjp10) * 100; end
if(gol_yjp10[31:0] == 0) begin err_im_per10 = (dut_im_yjp10 - gol_im_yjp10); end else begin err_im_per10 = ((dut_im_yjp10 - gol_im_yjp10) / gol_im_yjp10) * 100; end
if(gol_yjp11[63:32] == 0) begin err_re_per11 = (dut_re_yjp11 - gol_re_yjp11); end else begin err_re_per11 = ((dut_re_yjp11 - gol_re_yjp11) / gol_re_yjp11) * 100; end
if(gol_yjp11[31:0] == 0) begin err_im_per11 = (dut_im_yjp11 - gol_im_yjp11); end else begin err_im_per11 = ((dut_im_yjp11 - gol_im_yjp11) / gol_im_yjp11) * 100; end
if(gol_yjp12[63:32] == 0) begin err_re_per12 = (dut_re_yjp12 - gol_re_yjp12); end else begin err_re_per12 = ((dut_re_yjp12 - gol_re_yjp12) / gol_re_yjp12) * 100; end
if(gol_yjp12[31:0] == 0) begin err_im_per12 = (dut_im_yjp12 - gol_im_yjp12); end else begin err_im_per12 = ((dut_im_yjp12 - gol_im_yjp12) / gol_im_yjp12) * 100; end
if(gol_yjp13[63:32] == 0) begin err_re_per13 = (dut_re_yjp13 - gol_re_yjp13); end else begin err_re_per13 = ((dut_re_yjp13 - gol_re_yjp13) / gol_re_yjp13) * 100; end
if(gol_yjp13[31:0] == 0) begin err_im_per13 = (dut_im_yjp13 - gol_im_yjp13); end else begin err_im_per13 = ((dut_im_yjp13 - gol_im_yjp13) / gol_im_yjp13) * 100; end
if(gol_yjp14[63:32] == 0) begin err_re_per14 = (dut_re_yjp14 - gol_re_yjp14); end else begin err_re_per14 = ((dut_re_yjp14 - gol_re_yjp14) / gol_re_yjp14) * 100; end
if(gol_yjp14[31:0] == 0) begin err_im_per14 = (dut_im_yjp14 - gol_im_yjp14); end else begin err_im_per14 = ((dut_im_yjp14 - gol_im_yjp14) / gol_im_yjp14) * 100; end
if(gol_yjp15[63:32] == 0) begin err_re_per15 = (dut_re_yjp15 - gol_re_yjp15); end else begin err_re_per15 = ((dut_re_yjp15 - gol_re_yjp15) / gol_re_yjp15) * 100; end
if(gol_yjp15[31:0] == 0) begin err_im_per15 = (dut_im_yjp15 - gol_im_yjp15); end else begin err_im_per15 = ((dut_im_yjp15 - gol_im_yjp15) / gol_im_yjp15) * 100; end
if(gol_yjp16[63:32] == 0) begin err_re_per16 = (dut_re_yjp16 - gol_re_yjp16); end else begin err_re_per16 = ((dut_re_yjp16 - gol_re_yjp16) / gol_re_yjp16) * 100; end
if(gol_yjp16[31:0] == 0) begin err_im_per16 = (dut_im_yjp16 - gol_im_yjp16); end else begin err_im_per16 = ((dut_im_yjp16 - gol_im_yjp16) / gol_im_yjp16) * 100; end
if(gol_yjp17[63:32] == 0) begin err_re_per17 = (dut_re_yjp17 - gol_re_yjp17); end else begin err_re_per17 = ((dut_re_yjp17 - gol_re_yjp17) / gol_re_yjp17) * 100; end
if(gol_yjp17[31:0] == 0) begin err_im_per17 = (dut_im_yjp17 - gol_im_yjp17); end else begin err_im_per17 = ((dut_im_yjp17 - gol_im_yjp17) / gol_im_yjp17) * 100; end
if(gol_yjp18[63:32] == 0) begin err_re_per18 = (dut_re_yjp18 - gol_re_yjp18); end else begin err_re_per18 = ((dut_re_yjp18 - gol_re_yjp18) / gol_re_yjp18) * 100; end
if(gol_yjp18[31:0] == 0) begin err_im_per18 = (dut_im_yjp18 - gol_im_yjp18); end else begin err_im_per18 = ((dut_im_yjp18 - gol_im_yjp18) / gol_im_yjp18) * 100; end
if(gol_yjp19[63:32] == 0) begin err_re_per19 = (dut_re_yjp19 - gol_re_yjp19); end else begin err_re_per19 = ((dut_re_yjp19 - gol_re_yjp19) / gol_re_yjp19) * 100; end
if(gol_yjp19[31:0] == 0) begin err_im_per19 = (dut_im_yjp19 - gol_im_yjp19); end else begin err_im_per19 = ((dut_im_yjp19 - gol_im_yjp19) / gol_im_yjp19) * 100; end
if(gol_yjp20[63:32] == 0) begin err_re_per20 = (dut_re_yjp20 - gol_re_yjp20); end else begin err_re_per20 = ((dut_re_yjp20 - gol_re_yjp20) / gol_re_yjp20) * 100; end
if(gol_yjp20[31:0] == 0) begin err_im_per20 = (dut_im_yjp20 - gol_im_yjp20); end else begin err_im_per20 = ((dut_im_yjp20 - gol_im_yjp20) / gol_im_yjp20) * 100; end
if(gol_yjp21[63:32] == 0) begin err_re_per21 = (dut_re_yjp21 - gol_re_yjp21); end else begin err_re_per21 = ((dut_re_yjp21 - gol_re_yjp21) / gol_re_yjp21) * 100; end
if(gol_yjp21[31:0] == 0) begin err_im_per21 = (dut_im_yjp21 - gol_im_yjp21); end else begin err_im_per21 = ((dut_im_yjp21 - gol_im_yjp21) / gol_im_yjp21) * 100; end
if(gol_yjp22[63:32] == 0) begin err_re_per22 = (dut_re_yjp22 - gol_re_yjp22); end else begin err_re_per22 = ((dut_re_yjp22 - gol_re_yjp22) / gol_re_yjp22) * 100; end
if(gol_yjp22[31:0] == 0) begin err_im_per22 = (dut_im_yjp22 - gol_im_yjp22); end else begin err_im_per22 = ((dut_im_yjp22 - gol_im_yjp22) / gol_im_yjp22) * 100; end
if(gol_yjp23[63:32] == 0) begin err_re_per23 = (dut_re_yjp23 - gol_re_yjp23); end else begin err_re_per23 = ((dut_re_yjp23 - gol_re_yjp23) / gol_re_yjp23) * 100; end
if(gol_yjp23[31:0] == 0) begin err_im_per23 = (dut_im_yjp23 - gol_im_yjp23); end else begin err_im_per23 = ((dut_im_yjp23 - gol_im_yjp23) / gol_im_yjp23) * 100; end
if(gol_yjp24[63:32] == 0) begin err_re_per24 = (dut_re_yjp24 - gol_re_yjp24); end else begin err_re_per24 = ((dut_re_yjp24 - gol_re_yjp24) / gol_re_yjp24) * 100; end
if(gol_yjp24[31:0] == 0) begin err_im_per24 = (dut_im_yjp24 - gol_im_yjp24); end else begin err_im_per24 = ((dut_im_yjp24 - gol_im_yjp24) / gol_im_yjp24) * 100; end
if(gol_yjp25[63:32] == 0) begin err_re_per25 = (dut_re_yjp25 - gol_re_yjp25); end else begin err_re_per25 = ((dut_re_yjp25 - gol_re_yjp25) / gol_re_yjp25) * 100; end
if(gol_yjp25[31:0] == 0) begin err_im_per25 = (dut_im_yjp25 - gol_im_yjp25); end else begin err_im_per25 = ((dut_im_yjp25 - gol_im_yjp25) / gol_im_yjp25) * 100; end
if(gol_yjp26[63:32] == 0) begin err_re_per26 = (dut_re_yjp26 - gol_re_yjp26); end else begin err_re_per26 = ((dut_re_yjp26 - gol_re_yjp26) / gol_re_yjp26) * 100; end
if(gol_yjp26[31:0] == 0) begin err_im_per26 = (dut_im_yjp26 - gol_im_yjp26); end else begin err_im_per26 = ((dut_im_yjp26 - gol_im_yjp26) / gol_im_yjp26) * 100; end
if(gol_yjp27[63:32] == 0) begin err_re_per27 = (dut_re_yjp27 - gol_re_yjp27); end else begin err_re_per27 = ((dut_re_yjp27 - gol_re_yjp27) / gol_re_yjp27) * 100; end
if(gol_yjp27[31:0] == 0) begin err_im_per27 = (dut_im_yjp27 - gol_im_yjp27); end else begin err_im_per27 = ((dut_im_yjp27 - gol_im_yjp27) / gol_im_yjp27) * 100; end
if(gol_yjp28[63:32] == 0) begin err_re_per28 = (dut_re_yjp28 - gol_re_yjp28); end else begin err_re_per28 = ((dut_re_yjp28 - gol_re_yjp28) / gol_re_yjp28) * 100; end
if(gol_yjp28[31:0] == 0) begin err_im_per28 = (dut_im_yjp28 - gol_im_yjp28); end else begin err_im_per28 = ((dut_im_yjp28 - gol_im_yjp28) / gol_im_yjp28) * 100; end
if(gol_yjp29[63:32] == 0) begin err_re_per29 = (dut_re_yjp29 - gol_re_yjp29); end else begin err_re_per29 = ((dut_re_yjp29 - gol_re_yjp29) / gol_re_yjp29) * 100; end
if(gol_yjp29[31:0] == 0) begin err_im_per29 = (dut_im_yjp29 - gol_im_yjp29); end else begin err_im_per29 = ((dut_im_yjp29 - gol_im_yjp29) / gol_im_yjp29) * 100; end
if(gol_yjp30[63:32] == 0) begin err_re_per30 = (dut_re_yjp30 - gol_re_yjp30); end else begin err_re_per30 = ((dut_re_yjp30 - gol_re_yjp30) / gol_re_yjp30) * 100; end
if(gol_yjp30[31:0] == 0) begin err_im_per30 = (dut_im_yjp30 - gol_im_yjp30); end else begin err_im_per30 = ((dut_im_yjp30 - gol_im_yjp30) / gol_im_yjp30) * 100; end
if(gol_yjp31[63:32] == 0) begin err_re_per31 = (dut_re_yjp31 - gol_re_yjp31); end else begin err_re_per31 = ((dut_re_yjp31 - gol_re_yjp31) / gol_re_yjp31) * 100; end
if(gol_yjp31[31:0] == 0) begin err_im_per31 = (dut_im_yjp31 - gol_im_yjp31); end else begin err_im_per31 = ((dut_im_yjp31 - gol_im_yjp31) / gol_im_yjp31) * 100; end
if(gol_yjp32[63:32] == 0) begin err_re_per32 = (dut_re_yjp32 - gol_re_yjp32); end else begin err_re_per32 = ((dut_re_yjp32 - gol_re_yjp32) / gol_re_yjp32) * 100; end
if(gol_yjp32[31:0] == 0) begin err_im_per32 = (dut_im_yjp32 - gol_im_yjp32); end else begin err_im_per32 = ((dut_im_yjp32 - gol_im_yjp32) / gol_im_yjp32) * 100; end
if(gol_yjp33[63:32] == 0) begin err_re_per33 = (dut_re_yjp33 - gol_re_yjp33); end else begin err_re_per33 = ((dut_re_yjp33 - gol_re_yjp33) / gol_re_yjp33) * 100; end
if(gol_yjp33[31:0] == 0) begin err_im_per33 = (dut_im_yjp33 - gol_im_yjp33); end else begin err_im_per33 = ((dut_im_yjp33 - gol_im_yjp33) / gol_im_yjp33) * 100; end
if(gol_yjp34[63:32] == 0) begin err_re_per34 = (dut_re_yjp34 - gol_re_yjp34); end else begin err_re_per34 = ((dut_re_yjp34 - gol_re_yjp34) / gol_re_yjp34) * 100; end
if(gol_yjp34[31:0] == 0) begin err_im_per34 = (dut_im_yjp34 - gol_im_yjp34); end else begin err_im_per34 = ((dut_im_yjp34 - gol_im_yjp34) / gol_im_yjp34) * 100; end
if(gol_yjp35[63:32] == 0) begin err_re_per35 = (dut_re_yjp35 - gol_re_yjp35); end else begin err_re_per35 = ((dut_re_yjp35 - gol_re_yjp35) / gol_re_yjp35) * 100; end
if(gol_yjp35[31:0] == 0) begin err_im_per35 = (dut_im_yjp35 - gol_im_yjp35); end else begin err_im_per35 = ((dut_im_yjp35 - gol_im_yjp35) / gol_im_yjp35) * 100; end
if(gol_yjp36[63:32] == 0) begin err_re_per36 = (dut_re_yjp36 - gol_re_yjp36); end else begin err_re_per36 = ((dut_re_yjp36 - gol_re_yjp36) / gol_re_yjp36) * 100; end
if(gol_yjp36[31:0] == 0) begin err_im_per36 = (dut_im_yjp36 - gol_im_yjp36); end else begin err_im_per36 = ((dut_im_yjp36 - gol_im_yjp36) / gol_im_yjp36) * 100; end
if(gol_yjp37[63:32] == 0) begin err_re_per37 = (dut_re_yjp37 - gol_re_yjp37); end else begin err_re_per37 = ((dut_re_yjp37 - gol_re_yjp37) / gol_re_yjp37) * 100; end
if(gol_yjp37[31:0] == 0) begin err_im_per37 = (dut_im_yjp37 - gol_im_yjp37); end else begin err_im_per37 = ((dut_im_yjp37 - gol_im_yjp37) / gol_im_yjp37) * 100; end
if(gol_yjp38[63:32] == 0) begin err_re_per38 = (dut_re_yjp38 - gol_re_yjp38); end else begin err_re_per38 = ((dut_re_yjp38 - gol_re_yjp38) / gol_re_yjp38) * 100; end
if(gol_yjp38[31:0] == 0) begin err_im_per38 = (dut_im_yjp38 - gol_im_yjp38); end else begin err_im_per38 = ((dut_im_yjp38 - gol_im_yjp38) / gol_im_yjp38) * 100; end
if(gol_yjp39[63:32] == 0) begin err_re_per39 = (dut_re_yjp39 - gol_re_yjp39); end else begin err_re_per39 = ((dut_re_yjp39 - gol_re_yjp39) / gol_re_yjp39) * 100; end
if(gol_yjp39[31:0] == 0) begin err_im_per39 = (dut_im_yjp39 - gol_im_yjp39); end else begin err_im_per39 = ((dut_im_yjp39 - gol_im_yjp39) / gol_im_yjp39) * 100; end
if(gol_yjp40[63:32] == 0) begin err_re_per40 = (dut_re_yjp40 - gol_re_yjp40); end else begin err_re_per40 = ((dut_re_yjp40 - gol_re_yjp40) / gol_re_yjp40) * 100; end
if(gol_yjp40[31:0] == 0) begin err_im_per40 = (dut_im_yjp40 - gol_im_yjp40); end else begin err_im_per40 = ((dut_im_yjp40 - gol_im_yjp40) / gol_im_yjp40) * 100; end
if(gol_yjp41[63:32] == 0) begin err_re_per41 = (dut_re_yjp41 - gol_re_yjp41); end else begin err_re_per41 = ((dut_re_yjp41 - gol_re_yjp41) / gol_re_yjp41) * 100; end
if(gol_yjp41[31:0] == 0) begin err_im_per41 = (dut_im_yjp41 - gol_im_yjp41); end else begin err_im_per41 = ((dut_im_yjp41 - gol_im_yjp41) / gol_im_yjp41) * 100; end
if(gol_yjp42[63:32] == 0) begin err_re_per42 = (dut_re_yjp42 - gol_re_yjp42); end else begin err_re_per42 = ((dut_re_yjp42 - gol_re_yjp42) / gol_re_yjp42) * 100; end
if(gol_yjp42[31:0] == 0) begin err_im_per42 = (dut_im_yjp42 - gol_im_yjp42); end else begin err_im_per42 = ((dut_im_yjp42 - gol_im_yjp42) / gol_im_yjp42) * 100; end
if(gol_yjp43[63:32] == 0) begin err_re_per43 = (dut_re_yjp43 - gol_re_yjp43); end else begin err_re_per43 = ((dut_re_yjp43 - gol_re_yjp43) / gol_re_yjp43) * 100; end
if(gol_yjp43[31:0] == 0) begin err_im_per43 = (dut_im_yjp43 - gol_im_yjp43); end else begin err_im_per43 = ((dut_im_yjp43 - gol_im_yjp43) / gol_im_yjp43) * 100; end
if(gol_yjp44[63:32] == 0) begin err_re_per44 = (dut_re_yjp44 - gol_re_yjp44); end else begin err_re_per44 = ((dut_re_yjp44 - gol_re_yjp44) / gol_re_yjp44) * 100; end
if(gol_yjp44[31:0] == 0) begin err_im_per44 = (dut_im_yjp44 - gol_im_yjp44); end else begin err_im_per44 = ((dut_im_yjp44 - gol_im_yjp44) / gol_im_yjp44) * 100; end
if(gol_yjp45[63:32] == 0) begin err_re_per45 = (dut_re_yjp45 - gol_re_yjp45); end else begin err_re_per45 = ((dut_re_yjp45 - gol_re_yjp45) / gol_re_yjp45) * 100; end
if(gol_yjp45[31:0] == 0) begin err_im_per45 = (dut_im_yjp45 - gol_im_yjp45); end else begin err_im_per45 = ((dut_im_yjp45 - gol_im_yjp45) / gol_im_yjp45) * 100; end
if(gol_yjp46[63:32] == 0) begin err_re_per46 = (dut_re_yjp46 - gol_re_yjp46); end else begin err_re_per46 = ((dut_re_yjp46 - gol_re_yjp46) / gol_re_yjp46) * 100; end
if(gol_yjp46[31:0] == 0) begin err_im_per46 = (dut_im_yjp46 - gol_im_yjp46); end else begin err_im_per46 = ((dut_im_yjp46 - gol_im_yjp46) / gol_im_yjp46) * 100; end
if(gol_yjp47[63:32] == 0) begin err_re_per47 = (dut_re_yjp47 - gol_re_yjp47); end else begin err_re_per47 = ((dut_re_yjp47 - gol_re_yjp47) / gol_re_yjp47) * 100; end
if(gol_yjp47[31:0] == 0) begin err_im_per47 = (dut_im_yjp47 - gol_im_yjp47); end else begin err_im_per47 = ((dut_im_yjp47 - gol_im_yjp47) / gol_im_yjp47) * 100; end
if(gol_yjp48[63:32] == 0) begin err_re_per48 = (dut_re_yjp48 - gol_re_yjp48); end else begin err_re_per48 = ((dut_re_yjp48 - gol_re_yjp48) / gol_re_yjp48) * 100; end
if(gol_yjp48[31:0] == 0) begin err_im_per48 = (dut_im_yjp48 - gol_im_yjp48); end else begin err_im_per48 = ((dut_im_yjp48 - gol_im_yjp48) / gol_im_yjp48) * 100; end
if(gol_yjp49[63:32] == 0) begin err_re_per49 = (dut_re_yjp49 - gol_re_yjp49); end else begin err_re_per49 = ((dut_re_yjp49 - gol_re_yjp49) / gol_re_yjp49) * 100; end
if(gol_yjp49[31:0] == 0) begin err_im_per49 = (dut_im_yjp49 - gol_im_yjp49); end else begin err_im_per49 = ((dut_im_yjp49 - gol_im_yjp49) / gol_im_yjp49) * 100; end
if(gol_yjp50[63:32] == 0) begin err_re_per50 = (dut_re_yjp50 - gol_re_yjp50); end else begin err_re_per50 = ((dut_re_yjp50 - gol_re_yjp50) / gol_re_yjp50) * 100; end
if(gol_yjp50[31:0] == 0) begin err_im_per50 = (dut_im_yjp50 - gol_im_yjp50); end else begin err_im_per50 = ((dut_im_yjp50 - gol_im_yjp50) / gol_im_yjp50) * 100; end
if(gol_yjp51[63:32] == 0) begin err_re_per51 = (dut_re_yjp51 - gol_re_yjp51); end else begin err_re_per51 = ((dut_re_yjp51 - gol_re_yjp51) / gol_re_yjp51) * 100; end
if(gol_yjp51[31:0] == 0) begin err_im_per51 = (dut_im_yjp51 - gol_im_yjp51); end else begin err_im_per51 = ((dut_im_yjp51 - gol_im_yjp51) / gol_im_yjp51) * 100; end
if(gol_yjp52[63:32] == 0) begin err_re_per52 = (dut_re_yjp52 - gol_re_yjp52); end else begin err_re_per52 = ((dut_re_yjp52 - gol_re_yjp52) / gol_re_yjp52) * 100; end
if(gol_yjp52[31:0] == 0) begin err_im_per52 = (dut_im_yjp52 - gol_im_yjp52); end else begin err_im_per52 = ((dut_im_yjp52 - gol_im_yjp52) / gol_im_yjp52) * 100; end
if(gol_yjp53[63:32] == 0) begin err_re_per53 = (dut_re_yjp53 - gol_re_yjp53); end else begin err_re_per53 = ((dut_re_yjp53 - gol_re_yjp53) / gol_re_yjp53) * 100; end
if(gol_yjp53[31:0] == 0) begin err_im_per53 = (dut_im_yjp53 - gol_im_yjp53); end else begin err_im_per53 = ((dut_im_yjp53 - gol_im_yjp53) / gol_im_yjp53) * 100; end
if(gol_yjp54[63:32] == 0) begin err_re_per54 = (dut_re_yjp54 - gol_re_yjp54); end else begin err_re_per54 = ((dut_re_yjp54 - gol_re_yjp54) / gol_re_yjp54) * 100; end
if(gol_yjp54[31:0] == 0) begin err_im_per54 = (dut_im_yjp54 - gol_im_yjp54); end else begin err_im_per54 = ((dut_im_yjp54 - gol_im_yjp54) / gol_im_yjp54) * 100; end
if(gol_yjp55[63:32] == 0) begin err_re_per55 = (dut_re_yjp55 - gol_re_yjp55); end else begin err_re_per55 = ((dut_re_yjp55 - gol_re_yjp55) / gol_re_yjp55) * 100; end
if(gol_yjp55[31:0] == 0) begin err_im_per55 = (dut_im_yjp55 - gol_im_yjp55); end else begin err_im_per55 = ((dut_im_yjp55 - gol_im_yjp55) / gol_im_yjp55) * 100; end
if(gol_yjp56[63:32] == 0) begin err_re_per56 = (dut_re_yjp56 - gol_re_yjp56); end else begin err_re_per56 = ((dut_re_yjp56 - gol_re_yjp56) / gol_re_yjp56) * 100; end
if(gol_yjp56[31:0] == 0) begin err_im_per56 = (dut_im_yjp56 - gol_im_yjp56); end else begin err_im_per56 = ((dut_im_yjp56 - gol_im_yjp56) / gol_im_yjp56) * 100; end
if(gol_yjp57[63:32] == 0) begin err_re_per57 = (dut_re_yjp57 - gol_re_yjp57); end else begin err_re_per57 = ((dut_re_yjp57 - gol_re_yjp57) / gol_re_yjp57) * 100; end
if(gol_yjp57[31:0] == 0) begin err_im_per57 = (dut_im_yjp57 - gol_im_yjp57); end else begin err_im_per57 = ((dut_im_yjp57 - gol_im_yjp57) / gol_im_yjp57) * 100; end
if(gol_yjp58[63:32] == 0) begin err_re_per58 = (dut_re_yjp58 - gol_re_yjp58); end else begin err_re_per58 = ((dut_re_yjp58 - gol_re_yjp58) / gol_re_yjp58) * 100; end
if(gol_yjp58[31:0] == 0) begin err_im_per58 = (dut_im_yjp58 - gol_im_yjp58); end else begin err_im_per58 = ((dut_im_yjp58 - gol_im_yjp58) / gol_im_yjp58) * 100; end
if(gol_yjp59[63:32] == 0) begin err_re_per59 = (dut_re_yjp59 - gol_re_yjp59); end else begin err_re_per59 = ((dut_re_yjp59 - gol_re_yjp59) / gol_re_yjp59) * 100; end
if(gol_yjp59[31:0] == 0) begin err_im_per59 = (dut_im_yjp59 - gol_im_yjp59); end else begin err_im_per59 = ((dut_im_yjp59 - gol_im_yjp59) / gol_im_yjp59) * 100; end
if(gol_yjp60[63:32] == 0) begin err_re_per60 = (dut_re_yjp60 - gol_re_yjp60); end else begin err_re_per60 = ((dut_re_yjp60 - gol_re_yjp60) / gol_re_yjp60) * 100; end
if(gol_yjp60[31:0] == 0) begin err_im_per60 = (dut_im_yjp60 - gol_im_yjp60); end else begin err_im_per60 = ((dut_im_yjp60 - gol_im_yjp60) / gol_im_yjp60) * 100; end
if(gol_yjp61[63:32] == 0) begin err_re_per61 = (dut_re_yjp61 - gol_re_yjp61); end else begin err_re_per61 = ((dut_re_yjp61 - gol_re_yjp61) / gol_re_yjp61) * 100; end
if(gol_yjp61[31:0] == 0) begin err_im_per61 = (dut_im_yjp61 - gol_im_yjp61); end else begin err_im_per61 = ((dut_im_yjp61 - gol_im_yjp61) / gol_im_yjp61) * 100; end
if(gol_yjp62[63:32] == 0) begin err_re_per62 = (dut_re_yjp62 - gol_re_yjp62); end else begin err_re_per62 = ((dut_re_yjp62 - gol_re_yjp62) / gol_re_yjp62) * 100; end
if(gol_yjp62[31:0] == 0) begin err_im_per62 = (dut_im_yjp62 - gol_im_yjp62); end else begin err_im_per62 = ((dut_im_yjp62 - gol_im_yjp62) / gol_im_yjp62) * 100; end
if(gol_yjp63[63:32] == 0) begin err_re_per63 = (dut_re_yjp63 - gol_re_yjp63); end else begin err_re_per63 = ((dut_re_yjp63 - gol_re_yjp63) / gol_re_yjp63) * 100; end
if(gol_yjp63[31:0] == 0) begin err_im_per63 = (dut_im_yjp63 - gol_im_yjp63); end else begin err_im_per63 = ((dut_im_yjp63 - gol_im_yjp63) / gol_im_yjp63) * 100; end
if(gol_yjp64[63:32] == 0) begin err_re_per64 = (dut_re_yjp64 - gol_re_yjp64); end else begin err_re_per64 = ((dut_re_yjp64 - gol_re_yjp64) / gol_re_yjp64) * 100; end
if(gol_yjp64[31:0] == 0) begin err_im_per64 = (dut_im_yjp64 - gol_im_yjp64); end else begin err_im_per64 = ((dut_im_yjp64 - gol_im_yjp64) / gol_im_yjp64) * 100; end
if(gol_yjp65[63:32] == 0) begin err_re_per65 = (dut_re_yjp65 - gol_re_yjp65); end else begin err_re_per65 = ((dut_re_yjp65 - gol_re_yjp65) / gol_re_yjp65) * 100; end
if(gol_yjp65[31:0] == 0) begin err_im_per65 = (dut_im_yjp65 - gol_im_yjp65); end else begin err_im_per65 = ((dut_im_yjp65 - gol_im_yjp65) / gol_im_yjp65) * 100; end
if(gol_yjp66[63:32] == 0) begin err_re_per66 = (dut_re_yjp66 - gol_re_yjp66); end else begin err_re_per66 = ((dut_re_yjp66 - gol_re_yjp66) / gol_re_yjp66) * 100; end
if(gol_yjp66[31:0] == 0) begin err_im_per66 = (dut_im_yjp66 - gol_im_yjp66); end else begin err_im_per66 = ((dut_im_yjp66 - gol_im_yjp66) / gol_im_yjp66) * 100; end
if(gol_yjp67[63:32] == 0) begin err_re_per67 = (dut_re_yjp67 - gol_re_yjp67); end else begin err_re_per67 = ((dut_re_yjp67 - gol_re_yjp67) / gol_re_yjp67) * 100; end
if(gol_yjp67[31:0] == 0) begin err_im_per67 = (dut_im_yjp67 - gol_im_yjp67); end else begin err_im_per67 = ((dut_im_yjp67 - gol_im_yjp67) / gol_im_yjp67) * 100; end
if(gol_yjp68[63:32] == 0) begin err_re_per68 = (dut_re_yjp68 - gol_re_yjp68); end else begin err_re_per68 = ((dut_re_yjp68 - gol_re_yjp68) / gol_re_yjp68) * 100; end
if(gol_yjp68[31:0] == 0) begin err_im_per68 = (dut_im_yjp68 - gol_im_yjp68); end else begin err_im_per68 = ((dut_im_yjp68 - gol_im_yjp68) / gol_im_yjp68) * 100; end
if(gol_yjp69[63:32] == 0) begin err_re_per69 = (dut_re_yjp69 - gol_re_yjp69); end else begin err_re_per69 = ((dut_re_yjp69 - gol_re_yjp69) / gol_re_yjp69) * 100; end
if(gol_yjp69[31:0] == 0) begin err_im_per69 = (dut_im_yjp69 - gol_im_yjp69); end else begin err_im_per69 = ((dut_im_yjp69 - gol_im_yjp69) / gol_im_yjp69) * 100; end
if(gol_yjp70[63:32] == 0) begin err_re_per70 = (dut_re_yjp70 - gol_re_yjp70); end else begin err_re_per70 = ((dut_re_yjp70 - gol_re_yjp70) / gol_re_yjp70) * 100; end
if(gol_yjp70[31:0] == 0) begin err_im_per70 = (dut_im_yjp70 - gol_im_yjp70); end else begin err_im_per70 = ((dut_im_yjp70 - gol_im_yjp70) / gol_im_yjp70) * 100; end
if(gol_yjp71[63:32] == 0) begin err_re_per71 = (dut_re_yjp71 - gol_re_yjp71); end else begin err_re_per71 = ((dut_re_yjp71 - gol_re_yjp71) / gol_re_yjp71) * 100; end
if(gol_yjp71[31:0] == 0) begin err_im_per71 = (dut_im_yjp71 - gol_im_yjp71); end else begin err_im_per71 = ((dut_im_yjp71 - gol_im_yjp71) / gol_im_yjp71) * 100; end
if(gol_yjp72[63:32] == 0) begin err_re_per72 = (dut_re_yjp72 - gol_re_yjp72); end else begin err_re_per72 = ((dut_re_yjp72 - gol_re_yjp72) / gol_re_yjp72) * 100; end
if(gol_yjp72[31:0] == 0) begin err_im_per72 = (dut_im_yjp72 - gol_im_yjp72); end else begin err_im_per72 = ((dut_im_yjp72 - gol_im_yjp72) / gol_im_yjp72) * 100; end
if(gol_yjp73[63:32] == 0) begin err_re_per73 = (dut_re_yjp73 - gol_re_yjp73); end else begin err_re_per73 = ((dut_re_yjp73 - gol_re_yjp73) / gol_re_yjp73) * 100; end
if(gol_yjp73[31:0] == 0) begin err_im_per73 = (dut_im_yjp73 - gol_im_yjp73); end else begin err_im_per73 = ((dut_im_yjp73 - gol_im_yjp73) / gol_im_yjp73) * 100; end
if(gol_yjp74[63:32] == 0) begin err_re_per74 = (dut_re_yjp74 - gol_re_yjp74); end else begin err_re_per74 = ((dut_re_yjp74 - gol_re_yjp74) / gol_re_yjp74) * 100; end
if(gol_yjp74[31:0] == 0) begin err_im_per74 = (dut_im_yjp74 - gol_im_yjp74); end else begin err_im_per74 = ((dut_im_yjp74 - gol_im_yjp74) / gol_im_yjp74) * 100; end
if(gol_yjp75[63:32] == 0) begin err_re_per75 = (dut_re_yjp75 - gol_re_yjp75); end else begin err_re_per75 = ((dut_re_yjp75 - gol_re_yjp75) / gol_re_yjp75) * 100; end
if(gol_yjp75[31:0] == 0) begin err_im_per75 = (dut_im_yjp75 - gol_im_yjp75); end else begin err_im_per75 = ((dut_im_yjp75 - gol_im_yjp75) / gol_im_yjp75) * 100; end
if(gol_yjp76[63:32] == 0) begin err_re_per76 = (dut_re_yjp76 - gol_re_yjp76); end else begin err_re_per76 = ((dut_re_yjp76 - gol_re_yjp76) / gol_re_yjp76) * 100; end
if(gol_yjp76[31:0] == 0) begin err_im_per76 = (dut_im_yjp76 - gol_im_yjp76); end else begin err_im_per76 = ((dut_im_yjp76 - gol_im_yjp76) / gol_im_yjp76) * 100; end
if(gol_yjp77[63:32] == 0) begin err_re_per77 = (dut_re_yjp77 - gol_re_yjp77); end else begin err_re_per77 = ((dut_re_yjp77 - gol_re_yjp77) / gol_re_yjp77) * 100; end
if(gol_yjp77[31:0] == 0) begin err_im_per77 = (dut_im_yjp77 - gol_im_yjp77); end else begin err_im_per77 = ((dut_im_yjp77 - gol_im_yjp77) / gol_im_yjp77) * 100; end
if(gol_yjp78[63:32] == 0) begin err_re_per78 = (dut_re_yjp78 - gol_re_yjp78); end else begin err_re_per78 = ((dut_re_yjp78 - gol_re_yjp78) / gol_re_yjp78) * 100; end
if(gol_yjp78[31:0] == 0) begin err_im_per78 = (dut_im_yjp78 - gol_im_yjp78); end else begin err_im_per78 = ((dut_im_yjp78 - gol_im_yjp78) / gol_im_yjp78) * 100; end
if(gol_yjp79[63:32] == 0) begin err_re_per79 = (dut_re_yjp79 - gol_re_yjp79); end else begin err_re_per79 = ((dut_re_yjp79 - gol_re_yjp79) / gol_re_yjp79) * 100; end
if(gol_yjp79[31:0] == 0) begin err_im_per79 = (dut_im_yjp79 - gol_im_yjp79); end else begin err_im_per79 = ((dut_im_yjp79 - gol_im_yjp79) / gol_im_yjp79) * 100; end
if(gol_yjp80[63:32] == 0) begin err_re_per80 = (dut_re_yjp80 - gol_re_yjp80); end else begin err_re_per80 = ((dut_re_yjp80 - gol_re_yjp80) / gol_re_yjp80) * 100; end
if(gol_yjp80[31:0] == 0) begin err_im_per80 = (dut_im_yjp80 - gol_im_yjp80); end else begin err_im_per80 = ((dut_im_yjp80 - gol_im_yjp80) / gol_im_yjp80) * 100; end
if(gol_yjp81[63:32] == 0) begin err_re_per81 = (dut_re_yjp81 - gol_re_yjp81); end else begin err_re_per81 = ((dut_re_yjp81 - gol_re_yjp81) / gol_re_yjp81) * 100; end
if(gol_yjp81[31:0] == 0) begin err_im_per81 = (dut_im_yjp81 - gol_im_yjp81); end else begin err_im_per81 = ((dut_im_yjp81 - gol_im_yjp81) / gol_im_yjp81) * 100; end
if(gol_yjp82[63:32] == 0) begin err_re_per82 = (dut_re_yjp82 - gol_re_yjp82); end else begin err_re_per82 = ((dut_re_yjp82 - gol_re_yjp82) / gol_re_yjp82) * 100; end
if(gol_yjp82[31:0] == 0) begin err_im_per82 = (dut_im_yjp82 - gol_im_yjp82); end else begin err_im_per82 = ((dut_im_yjp82 - gol_im_yjp82) / gol_im_yjp82) * 100; end
if(gol_yjp83[63:32] == 0) begin err_re_per83 = (dut_re_yjp83 - gol_re_yjp83); end else begin err_re_per83 = ((dut_re_yjp83 - gol_re_yjp83) / gol_re_yjp83) * 100; end
if(gol_yjp83[31:0] == 0) begin err_im_per83 = (dut_im_yjp83 - gol_im_yjp83); end else begin err_im_per83 = ((dut_im_yjp83 - gol_im_yjp83) / gol_im_yjp83) * 100; end
if(gol_yjp84[63:32] == 0) begin err_re_per84 = (dut_re_yjp84 - gol_re_yjp84); end else begin err_re_per84 = ((dut_re_yjp84 - gol_re_yjp84) / gol_re_yjp84) * 100; end
if(gol_yjp84[31:0] == 0) begin err_im_per84 = (dut_im_yjp84 - gol_im_yjp84); end else begin err_im_per84 = ((dut_im_yjp84 - gol_im_yjp84) / gol_im_yjp84) * 100; end
if(gol_yjp85[63:32] == 0) begin err_re_per85 = (dut_re_yjp85 - gol_re_yjp85); end else begin err_re_per85 = ((dut_re_yjp85 - gol_re_yjp85) / gol_re_yjp85) * 100; end
if(gol_yjp85[31:0] == 0) begin err_im_per85 = (dut_im_yjp85 - gol_im_yjp85); end else begin err_im_per85 = ((dut_im_yjp85 - gol_im_yjp85) / gol_im_yjp85) * 100; end
if(gol_yjp86[63:32] == 0) begin err_re_per86 = (dut_re_yjp86 - gol_re_yjp86); end else begin err_re_per86 = ((dut_re_yjp86 - gol_re_yjp86) / gol_re_yjp86) * 100; end
if(gol_yjp86[31:0] == 0) begin err_im_per86 = (dut_im_yjp86 - gol_im_yjp86); end else begin err_im_per86 = ((dut_im_yjp86 - gol_im_yjp86) / gol_im_yjp86) * 100; end
if(gol_yjp87[63:32] == 0) begin err_re_per87 = (dut_re_yjp87 - gol_re_yjp87); end else begin err_re_per87 = ((dut_re_yjp87 - gol_re_yjp87) / gol_re_yjp87) * 100; end
if(gol_yjp87[31:0] == 0) begin err_im_per87 = (dut_im_yjp87 - gol_im_yjp87); end else begin err_im_per87 = ((dut_im_yjp87 - gol_im_yjp87) / gol_im_yjp87) * 100; end
if(gol_yjp88[63:32] == 0) begin err_re_per88 = (dut_re_yjp88 - gol_re_yjp88); end else begin err_re_per88 = ((dut_re_yjp88 - gol_re_yjp88) / gol_re_yjp88) * 100; end
if(gol_yjp88[31:0] == 0) begin err_im_per88 = (dut_im_yjp88 - gol_im_yjp88); end else begin err_im_per88 = ((dut_im_yjp88 - gol_im_yjp88) / gol_im_yjp88) * 100; end
if(gol_yjp89[63:32] == 0) begin err_re_per89 = (dut_re_yjp89 - gol_re_yjp89); end else begin err_re_per89 = ((dut_re_yjp89 - gol_re_yjp89) / gol_re_yjp89) * 100; end
if(gol_yjp89[31:0] == 0) begin err_im_per89 = (dut_im_yjp89 - gol_im_yjp89); end else begin err_im_per89 = ((dut_im_yjp89 - gol_im_yjp89) / gol_im_yjp89) * 100; end
if(gol_yjp90[63:32] == 0) begin err_re_per90 = (dut_re_yjp90 - gol_re_yjp90); end else begin err_re_per90 = ((dut_re_yjp90 - gol_re_yjp90) / gol_re_yjp90) * 100; end
if(gol_yjp90[31:0] == 0) begin err_im_per90 = (dut_im_yjp90 - gol_im_yjp90); end else begin err_im_per90 = ((dut_im_yjp90 - gol_im_yjp90) / gol_im_yjp90) * 100; end
if(gol_yjp91[63:32] == 0) begin err_re_per91 = (dut_re_yjp91 - gol_re_yjp91); end else begin err_re_per91 = ((dut_re_yjp91 - gol_re_yjp91) / gol_re_yjp91) * 100; end
if(gol_yjp91[31:0] == 0) begin err_im_per91 = (dut_im_yjp91 - gol_im_yjp91); end else begin err_im_per91 = ((dut_im_yjp91 - gol_im_yjp91) / gol_im_yjp91) * 100; end
if(gol_yjp92[63:32] == 0) begin err_re_per92 = (dut_re_yjp92 - gol_re_yjp92); end else begin err_re_per92 = ((dut_re_yjp92 - gol_re_yjp92) / gol_re_yjp92) * 100; end
if(gol_yjp92[31:0] == 0) begin err_im_per92 = (dut_im_yjp92 - gol_im_yjp92); end else begin err_im_per92 = ((dut_im_yjp92 - gol_im_yjp92) / gol_im_yjp92) * 100; end
if(gol_yjp93[63:32] == 0) begin err_re_per93 = (dut_re_yjp93 - gol_re_yjp93); end else begin err_re_per93 = ((dut_re_yjp93 - gol_re_yjp93) / gol_re_yjp93) * 100; end
if(gol_yjp93[31:0] == 0) begin err_im_per93 = (dut_im_yjp93 - gol_im_yjp93); end else begin err_im_per93 = ((dut_im_yjp93 - gol_im_yjp93) / gol_im_yjp93) * 100; end
if(gol_yjp94[63:32] == 0) begin err_re_per94 = (dut_re_yjp94 - gol_re_yjp94); end else begin err_re_per94 = ((dut_re_yjp94 - gol_re_yjp94) / gol_re_yjp94) * 100; end
if(gol_yjp94[31:0] == 0) begin err_im_per94 = (dut_im_yjp94 - gol_im_yjp94); end else begin err_im_per94 = ((dut_im_yjp94 - gol_im_yjp94) / gol_im_yjp94) * 100; end
if(gol_yjp95[63:32] == 0) begin err_re_per95 = (dut_re_yjp95 - gol_re_yjp95); end else begin err_re_per95 = ((dut_re_yjp95 - gol_re_yjp95) / gol_re_yjp95) * 100; end
if(gol_yjp95[31:0] == 0) begin err_im_per95 = (dut_im_yjp95 - gol_im_yjp95); end else begin err_im_per95 = ((dut_im_yjp95 - gol_im_yjp95) / gol_im_yjp95) * 100; end
if(gol_yjp96[63:32] == 0) begin err_re_per96 = (dut_re_yjp96 - gol_re_yjp96); end else begin err_re_per96 = ((dut_re_yjp96 - gol_re_yjp96) / gol_re_yjp96) * 100; end
if(gol_yjp96[31:0] == 0) begin err_im_per96 = (dut_im_yjp96 - gol_im_yjp96); end else begin err_im_per96 = ((dut_im_yjp96 - gol_im_yjp96) / gol_im_yjp96) * 100; end
if(gol_yjp97[63:32] == 0) begin err_re_per97 = (dut_re_yjp97 - gol_re_yjp97); end else begin err_re_per97 = ((dut_re_yjp97 - gol_re_yjp97) / gol_re_yjp97) * 100; end
if(gol_yjp97[31:0] == 0) begin err_im_per97 = (dut_im_yjp97 - gol_im_yjp97); end else begin err_im_per97 = ((dut_im_yjp97 - gol_im_yjp97) / gol_im_yjp97) * 100; end
if(gol_yjp98[63:32] == 0) begin err_re_per98 = (dut_re_yjp98 - gol_re_yjp98); end else begin err_re_per98 = ((dut_re_yjp98 - gol_re_yjp98) / gol_re_yjp98) * 100; end
if(gol_yjp98[31:0] == 0) begin err_im_per98 = (dut_im_yjp98 - gol_im_yjp98); end else begin err_im_per98 = ((dut_im_yjp98 - gol_im_yjp98) / gol_im_yjp98) * 100; end
if(gol_yjp99[63:32] == 0) begin err_re_per99 = (dut_re_yjp99 - gol_re_yjp99); end else begin err_re_per99 = ((dut_re_yjp99 - gol_re_yjp99) / gol_re_yjp99) * 100; end
if(gol_yjp99[31:0] == 0) begin err_im_per99 = (dut_im_yjp99 - gol_im_yjp99); end else begin err_im_per99 = ((dut_im_yjp99 - gol_im_yjp99) / gol_im_yjp99) * 100; end
if(gol_yjp100[63:32] == 0) begin err_re_per100 = (dut_re_yjp100 - gol_re_yjp100); end else begin err_re_per100 = ((dut_re_yjp100 - gol_re_yjp100) / gol_re_yjp100) * 100; end
if(gol_yjp100[31:0] == 0) begin err_im_per100 = (dut_im_yjp100 - gol_im_yjp100); end else begin err_im_per100 = ((dut_im_yjp100 - gol_im_yjp100) / gol_im_yjp100) * 100; end
if(gol_yjp101[63:32] == 0) begin err_re_per101 = (dut_re_yjp101 - gol_re_yjp101); end else begin err_re_per101 = ((dut_re_yjp101 - gol_re_yjp101) / gol_re_yjp101) * 100; end
if(gol_yjp101[31:0] == 0) begin err_im_per101 = (dut_im_yjp101 - gol_im_yjp101); end else begin err_im_per101 = ((dut_im_yjp101 - gol_im_yjp101) / gol_im_yjp101) * 100; end
if(gol_yjp102[63:32] == 0) begin err_re_per102 = (dut_re_yjp102 - gol_re_yjp102); end else begin err_re_per102 = ((dut_re_yjp102 - gol_re_yjp102) / gol_re_yjp102) * 100; end
if(gol_yjp102[31:0] == 0) begin err_im_per102 = (dut_im_yjp102 - gol_im_yjp102); end else begin err_im_per102 = ((dut_im_yjp102 - gol_im_yjp102) / gol_im_yjp102) * 100; end
if(gol_yjp103[63:32] == 0) begin err_re_per103 = (dut_re_yjp103 - gol_re_yjp103); end else begin err_re_per103 = ((dut_re_yjp103 - gol_re_yjp103) / gol_re_yjp103) * 100; end
if(gol_yjp103[31:0] == 0) begin err_im_per103 = (dut_im_yjp103 - gol_im_yjp103); end else begin err_im_per103 = ((dut_im_yjp103 - gol_im_yjp103) / gol_im_yjp103) * 100; end
if(gol_yjp104[63:32] == 0) begin err_re_per104 = (dut_re_yjp104 - gol_re_yjp104); end else begin err_re_per104 = ((dut_re_yjp104 - gol_re_yjp104) / gol_re_yjp104) * 100; end
if(gol_yjp104[31:0] == 0) begin err_im_per104 = (dut_im_yjp104 - gol_im_yjp104); end else begin err_im_per104 = ((dut_im_yjp104 - gol_im_yjp104) / gol_im_yjp104) * 100; end
if(gol_yjp105[63:32] == 0) begin err_re_per105 = (dut_re_yjp105 - gol_re_yjp105); end else begin err_re_per105 = ((dut_re_yjp105 - gol_re_yjp105) / gol_re_yjp105) * 100; end
if(gol_yjp105[31:0] == 0) begin err_im_per105 = (dut_im_yjp105 - gol_im_yjp105); end else begin err_im_per105 = ((dut_im_yjp105 - gol_im_yjp105) / gol_im_yjp105) * 100; end
if(gol_yjp106[63:32] == 0) begin err_re_per106 = (dut_re_yjp106 - gol_re_yjp106); end else begin err_re_per106 = ((dut_re_yjp106 - gol_re_yjp106) / gol_re_yjp106) * 100; end
if(gol_yjp106[31:0] == 0) begin err_im_per106 = (dut_im_yjp106 - gol_im_yjp106); end else begin err_im_per106 = ((dut_im_yjp106 - gol_im_yjp106) / gol_im_yjp106) * 100; end
if(gol_yjp107[63:32] == 0) begin err_re_per107 = (dut_re_yjp107 - gol_re_yjp107); end else begin err_re_per107 = ((dut_re_yjp107 - gol_re_yjp107) / gol_re_yjp107) * 100; end
if(gol_yjp107[31:0] == 0) begin err_im_per107 = (dut_im_yjp107 - gol_im_yjp107); end else begin err_im_per107 = ((dut_im_yjp107 - gol_im_yjp107) / gol_im_yjp107) * 100; end
if(gol_yjp108[63:32] == 0) begin err_re_per108 = (dut_re_yjp108 - gol_re_yjp108); end else begin err_re_per108 = ((dut_re_yjp108 - gol_re_yjp108) / gol_re_yjp108) * 100; end
if(gol_yjp108[31:0] == 0) begin err_im_per108 = (dut_im_yjp108 - gol_im_yjp108); end else begin err_im_per108 = ((dut_im_yjp108 - gol_im_yjp108) / gol_im_yjp108) * 100; end
if(gol_yjp109[63:32] == 0) begin err_re_per109 = (dut_re_yjp109 - gol_re_yjp109); end else begin err_re_per109 = ((dut_re_yjp109 - gol_re_yjp109) / gol_re_yjp109) * 100; end
if(gol_yjp109[31:0] == 0) begin err_im_per109 = (dut_im_yjp109 - gol_im_yjp109); end else begin err_im_per109 = ((dut_im_yjp109 - gol_im_yjp109) / gol_im_yjp109) * 100; end
if(gol_yjp110[63:32] == 0) begin err_re_per110 = (dut_re_yjp110 - gol_re_yjp110); end else begin err_re_per110 = ((dut_re_yjp110 - gol_re_yjp110) / gol_re_yjp110) * 100; end
if(gol_yjp110[31:0] == 0) begin err_im_per110 = (dut_im_yjp110 - gol_im_yjp110); end else begin err_im_per110 = ((dut_im_yjp110 - gol_im_yjp110) / gol_im_yjp110) * 100; end
if(gol_yjp111[63:32] == 0) begin err_re_per111 = (dut_re_yjp111 - gol_re_yjp111); end else begin err_re_per111 = ((dut_re_yjp111 - gol_re_yjp111) / gol_re_yjp111) * 100; end
if(gol_yjp111[31:0] == 0) begin err_im_per111 = (dut_im_yjp111 - gol_im_yjp111); end else begin err_im_per111 = ((dut_im_yjp111 - gol_im_yjp111) / gol_im_yjp111) * 100; end
if(gol_yjp112[63:32] == 0) begin err_re_per112 = (dut_re_yjp112 - gol_re_yjp112); end else begin err_re_per112 = ((dut_re_yjp112 - gol_re_yjp112) / gol_re_yjp112) * 100; end
if(gol_yjp112[31:0] == 0) begin err_im_per112 = (dut_im_yjp112 - gol_im_yjp112); end else begin err_im_per112 = ((dut_im_yjp112 - gol_im_yjp112) / gol_im_yjp112) * 100; end
if(gol_yjp113[63:32] == 0) begin err_re_per113 = (dut_re_yjp113 - gol_re_yjp113); end else begin err_re_per113 = ((dut_re_yjp113 - gol_re_yjp113) / gol_re_yjp113) * 100; end
if(gol_yjp113[31:0] == 0) begin err_im_per113 = (dut_im_yjp113 - gol_im_yjp113); end else begin err_im_per113 = ((dut_im_yjp113 - gol_im_yjp113) / gol_im_yjp113) * 100; end
if(gol_yjp114[63:32] == 0) begin err_re_per114 = (dut_re_yjp114 - gol_re_yjp114); end else begin err_re_per114 = ((dut_re_yjp114 - gol_re_yjp114) / gol_re_yjp114) * 100; end
if(gol_yjp114[31:0] == 0) begin err_im_per114 = (dut_im_yjp114 - gol_im_yjp114); end else begin err_im_per114 = ((dut_im_yjp114 - gol_im_yjp114) / gol_im_yjp114) * 100; end
if(gol_yjp115[63:32] == 0) begin err_re_per115 = (dut_re_yjp115 - gol_re_yjp115); end else begin err_re_per115 = ((dut_re_yjp115 - gol_re_yjp115) / gol_re_yjp115) * 100; end
if(gol_yjp115[31:0] == 0) begin err_im_per115 = (dut_im_yjp115 - gol_im_yjp115); end else begin err_im_per115 = ((dut_im_yjp115 - gol_im_yjp115) / gol_im_yjp115) * 100; end
if(gol_yjp116[63:32] == 0) begin err_re_per116 = (dut_re_yjp116 - gol_re_yjp116); end else begin err_re_per116 = ((dut_re_yjp116 - gol_re_yjp116) / gol_re_yjp116) * 100; end
if(gol_yjp116[31:0] == 0) begin err_im_per116 = (dut_im_yjp116 - gol_im_yjp116); end else begin err_im_per116 = ((dut_im_yjp116 - gol_im_yjp116) / gol_im_yjp116) * 100; end
if(gol_yjp117[63:32] == 0) begin err_re_per117 = (dut_re_yjp117 - gol_re_yjp117); end else begin err_re_per117 = ((dut_re_yjp117 - gol_re_yjp117) / gol_re_yjp117) * 100; end
if(gol_yjp117[31:0] == 0) begin err_im_per117 = (dut_im_yjp117 - gol_im_yjp117); end else begin err_im_per117 = ((dut_im_yjp117 - gol_im_yjp117) / gol_im_yjp117) * 100; end
if(gol_yjp118[63:32] == 0) begin err_re_per118 = (dut_re_yjp118 - gol_re_yjp118); end else begin err_re_per118 = ((dut_re_yjp118 - gol_re_yjp118) / gol_re_yjp118) * 100; end
if(gol_yjp118[31:0] == 0) begin err_im_per118 = (dut_im_yjp118 - gol_im_yjp118); end else begin err_im_per118 = ((dut_im_yjp118 - gol_im_yjp118) / gol_im_yjp118) * 100; end
if(gol_yjp119[63:32] == 0) begin err_re_per119 = (dut_re_yjp119 - gol_re_yjp119); end else begin err_re_per119 = ((dut_re_yjp119 - gol_re_yjp119) / gol_re_yjp119) * 100; end
if(gol_yjp119[31:0] == 0) begin err_im_per119 = (dut_im_yjp119 - gol_im_yjp119); end else begin err_im_per119 = ((dut_im_yjp119 - gol_im_yjp119) / gol_im_yjp119) * 100; end
if(gol_yjp120[63:32] == 0) begin err_re_per120 = (dut_re_yjp120 - gol_re_yjp120); end else begin err_re_per120 = ((dut_re_yjp120 - gol_re_yjp120) / gol_re_yjp120) * 100; end
if(gol_yjp120[31:0] == 0) begin err_im_per120 = (dut_im_yjp120 - gol_im_yjp120); end else begin err_im_per120 = ((dut_im_yjp120 - gol_im_yjp120) / gol_im_yjp120) * 100; end
if(gol_yjp121[63:32] == 0) begin err_re_per121 = (dut_re_yjp121 - gol_re_yjp121); end else begin err_re_per121 = ((dut_re_yjp121 - gol_re_yjp121) / gol_re_yjp121) * 100; end
if(gol_yjp121[31:0] == 0) begin err_im_per121 = (dut_im_yjp121 - gol_im_yjp121); end else begin err_im_per121 = ((dut_im_yjp121 - gol_im_yjp121) / gol_im_yjp121) * 100; end
if(gol_yjp122[63:32] == 0) begin err_re_per122 = (dut_re_yjp122 - gol_re_yjp122); end else begin err_re_per122 = ((dut_re_yjp122 - gol_re_yjp122) / gol_re_yjp122) * 100; end
if(gol_yjp122[31:0] == 0) begin err_im_per122 = (dut_im_yjp122 - gol_im_yjp122); end else begin err_im_per122 = ((dut_im_yjp122 - gol_im_yjp122) / gol_im_yjp122) * 100; end
if(gol_yjp123[63:32] == 0) begin err_re_per123 = (dut_re_yjp123 - gol_re_yjp123); end else begin err_re_per123 = ((dut_re_yjp123 - gol_re_yjp123) / gol_re_yjp123) * 100; end
if(gol_yjp123[31:0] == 0) begin err_im_per123 = (dut_im_yjp123 - gol_im_yjp123); end else begin err_im_per123 = ((dut_im_yjp123 - gol_im_yjp123) / gol_im_yjp123) * 100; end
if(gol_yjp124[63:32] == 0) begin err_re_per124 = (dut_re_yjp124 - gol_re_yjp124); end else begin err_re_per124 = ((dut_re_yjp124 - gol_re_yjp124) / gol_re_yjp124) * 100; end
if(gol_yjp124[31:0] == 0) begin err_im_per124 = (dut_im_yjp124 - gol_im_yjp124); end else begin err_im_per124 = ((dut_im_yjp124 - gol_im_yjp124) / gol_im_yjp124) * 100; end
if(gol_yjp125[63:32] == 0) begin err_re_per125 = (dut_re_yjp125 - gol_re_yjp125); end else begin err_re_per125 = ((dut_re_yjp125 - gol_re_yjp125) / gol_re_yjp125) * 100; end
if(gol_yjp125[31:0] == 0) begin err_im_per125 = (dut_im_yjp125 - gol_im_yjp125); end else begin err_im_per125 = ((dut_im_yjp125 - gol_im_yjp125) / gol_im_yjp125) * 100; end
if(gol_yjp126[63:32] == 0) begin err_re_per126 = (dut_re_yjp126 - gol_re_yjp126); end else begin err_re_per126 = ((dut_re_yjp126 - gol_re_yjp126) / gol_re_yjp126) * 100; end
if(gol_yjp126[31:0] == 0) begin err_im_per126 = (dut_im_yjp126 - gol_im_yjp126); end else begin err_im_per126 = ((dut_im_yjp126 - gol_im_yjp126) / gol_im_yjp126) * 100; end
if(gol_yjp127[63:32] == 0) begin err_re_per127 = (dut_re_yjp127 - gol_re_yjp127); end else begin err_re_per127 = ((dut_re_yjp127 - gol_re_yjp127) / gol_re_yjp127) * 100; end
if(gol_yjp127[31:0] == 0) begin err_im_per127 = (dut_im_yjp127 - gol_im_yjp127); end else begin err_im_per127 = ((dut_im_yjp127 - gol_im_yjp127) / gol_im_yjp127) * 100; end
if(gol_yjp128[63:32] == 0) begin err_re_per128 = (dut_re_yjp128 - gol_re_yjp128); end else begin err_re_per128 = ((dut_re_yjp128 - gol_re_yjp128) / gol_re_yjp128) * 100; end
if(gol_yjp128[31:0] == 0) begin err_im_per128 = (dut_im_yjp128 - gol_im_yjp128); end else begin err_im_per128 = ((dut_im_yjp128 - gol_im_yjp128) / gol_im_yjp128) * 100; end
if(gol_yjp129[63:32] == 0) begin err_re_per129 = (dut_re_yjp129 - gol_re_yjp129); end else begin err_re_per129 = ((dut_re_yjp129 - gol_re_yjp129) / gol_re_yjp129) * 100; end
if(gol_yjp129[31:0] == 0) begin err_im_per129 = (dut_im_yjp129 - gol_im_yjp129); end else begin err_im_per129 = ((dut_im_yjp129 - gol_im_yjp129) / gol_im_yjp129) * 100; end
if(gol_yjp130[63:32] == 0) begin err_re_per130 = (dut_re_yjp130 - gol_re_yjp130); end else begin err_re_per130 = ((dut_re_yjp130 - gol_re_yjp130) / gol_re_yjp130) * 100; end
if(gol_yjp130[31:0] == 0) begin err_im_per130 = (dut_im_yjp130 - gol_im_yjp130); end else begin err_im_per130 = ((dut_im_yjp130 - gol_im_yjp130) / gol_im_yjp130) * 100; end
if(gol_yjp131[63:32] == 0) begin err_re_per131 = (dut_re_yjp131 - gol_re_yjp131); end else begin err_re_per131 = ((dut_re_yjp131 - gol_re_yjp131) / gol_re_yjp131) * 100; end
if(gol_yjp131[31:0] == 0) begin err_im_per131 = (dut_im_yjp131 - gol_im_yjp131); end else begin err_im_per131 = ((dut_im_yjp131 - gol_im_yjp131) / gol_im_yjp131) * 100; end
if(gol_yjp132[63:32] == 0) begin err_re_per132 = (dut_re_yjp132 - gol_re_yjp132); end else begin err_re_per132 = ((dut_re_yjp132 - gol_re_yjp132) / gol_re_yjp132) * 100; end
if(gol_yjp132[31:0] == 0) begin err_im_per132 = (dut_im_yjp132 - gol_im_yjp132); end else begin err_im_per132 = ((dut_im_yjp132 - gol_im_yjp132) / gol_im_yjp132) * 100; end
if(gol_yjp133[63:32] == 0) begin err_re_per133 = (dut_re_yjp133 - gol_re_yjp133); end else begin err_re_per133 = ((dut_re_yjp133 - gol_re_yjp133) / gol_re_yjp133) * 100; end
if(gol_yjp133[31:0] == 0) begin err_im_per133 = (dut_im_yjp133 - gol_im_yjp133); end else begin err_im_per133 = ((dut_im_yjp133 - gol_im_yjp133) / gol_im_yjp133) * 100; end
if(gol_yjp134[63:32] == 0) begin err_re_per134 = (dut_re_yjp134 - gol_re_yjp134); end else begin err_re_per134 = ((dut_re_yjp134 - gol_re_yjp134) / gol_re_yjp134) * 100; end
if(gol_yjp134[31:0] == 0) begin err_im_per134 = (dut_im_yjp134 - gol_im_yjp134); end else begin err_im_per134 = ((dut_im_yjp134 - gol_im_yjp134) / gol_im_yjp134) * 100; end
if(gol_yjp135[63:32] == 0) begin err_re_per135 = (dut_re_yjp135 - gol_re_yjp135); end else begin err_re_per135 = ((dut_re_yjp135 - gol_re_yjp135) / gol_re_yjp135) * 100; end
if(gol_yjp135[31:0] == 0) begin err_im_per135 = (dut_im_yjp135 - gol_im_yjp135); end else begin err_im_per135 = ((dut_im_yjp135 - gol_im_yjp135) / gol_im_yjp135) * 100; end
if(gol_yjp136[63:32] == 0) begin err_re_per136 = (dut_re_yjp136 - gol_re_yjp136); end else begin err_re_per136 = ((dut_re_yjp136 - gol_re_yjp136) / gol_re_yjp136) * 100; end
if(gol_yjp136[31:0] == 0) begin err_im_per136 = (dut_im_yjp136 - gol_im_yjp136); end else begin err_im_per136 = ((dut_im_yjp136 - gol_im_yjp136) / gol_im_yjp136) * 100; end
if(gol_yjp137[63:32] == 0) begin err_re_per137 = (dut_re_yjp137 - gol_re_yjp137); end else begin err_re_per137 = ((dut_re_yjp137 - gol_re_yjp137) / gol_re_yjp137) * 100; end
if(gol_yjp137[31:0] == 0) begin err_im_per137 = (dut_im_yjp137 - gol_im_yjp137); end else begin err_im_per137 = ((dut_im_yjp137 - gol_im_yjp137) / gol_im_yjp137) * 100; end
if(gol_yjp138[63:32] == 0) begin err_re_per138 = (dut_re_yjp138 - gol_re_yjp138); end else begin err_re_per138 = ((dut_re_yjp138 - gol_re_yjp138) / gol_re_yjp138) * 100; end
if(gol_yjp138[31:0] == 0) begin err_im_per138 = (dut_im_yjp138 - gol_im_yjp138); end else begin err_im_per138 = ((dut_im_yjp138 - gol_im_yjp138) / gol_im_yjp138) * 100; end
if(gol_yjp139[63:32] == 0) begin err_re_per139 = (dut_re_yjp139 - gol_re_yjp139); end else begin err_re_per139 = ((dut_re_yjp139 - gol_re_yjp139) / gol_re_yjp139) * 100; end
if(gol_yjp139[31:0] == 0) begin err_im_per139 = (dut_im_yjp139 - gol_im_yjp139); end else begin err_im_per139 = ((dut_im_yjp139 - gol_im_yjp139) / gol_im_yjp139) * 100; end
if(gol_yjp140[63:32] == 0) begin err_re_per140 = (dut_re_yjp140 - gol_re_yjp140); end else begin err_re_per140 = ((dut_re_yjp140 - gol_re_yjp140) / gol_re_yjp140) * 100; end
if(gol_yjp140[31:0] == 0) begin err_im_per140 = (dut_im_yjp140 - gol_im_yjp140); end else begin err_im_per140 = ((dut_im_yjp140 - gol_im_yjp140) / gol_im_yjp140) * 100; end
if(gol_yjp141[63:32] == 0) begin err_re_per141 = (dut_re_yjp141 - gol_re_yjp141); end else begin err_re_per141 = ((dut_re_yjp141 - gol_re_yjp141) / gol_re_yjp141) * 100; end
if(gol_yjp141[31:0] == 0) begin err_im_per141 = (dut_im_yjp141 - gol_im_yjp141); end else begin err_im_per141 = ((dut_im_yjp141 - gol_im_yjp141) / gol_im_yjp141) * 100; end
if(gol_yjp142[63:32] == 0) begin err_re_per142 = (dut_re_yjp142 - gol_re_yjp142); end else begin err_re_per142 = ((dut_re_yjp142 - gol_re_yjp142) / gol_re_yjp142) * 100; end
if(gol_yjp142[31:0] == 0) begin err_im_per142 = (dut_im_yjp142 - gol_im_yjp142); end else begin err_im_per142 = ((dut_im_yjp142 - gol_im_yjp142) / gol_im_yjp142) * 100; end
if(gol_yjp143[63:32] == 0) begin err_re_per143 = (dut_re_yjp143 - gol_re_yjp143); end else begin err_re_per143 = ((dut_re_yjp143 - gol_re_yjp143) / gol_re_yjp143) * 100; end
if(gol_yjp143[31:0] == 0) begin err_im_per143 = (dut_im_yjp143 - gol_im_yjp143); end else begin err_im_per143 = ((dut_im_yjp143 - gol_im_yjp143) / gol_im_yjp143) * 100; end
if(gol_yjp144[63:32] == 0) begin err_re_per144 = (dut_re_yjp144 - gol_re_yjp144); end else begin err_re_per144 = ((dut_re_yjp144 - gol_re_yjp144) / gol_re_yjp144) * 100; end
if(gol_yjp144[31:0] == 0) begin err_im_per144 = (dut_im_yjp144 - gol_im_yjp144); end else begin err_im_per144 = ((dut_im_yjp144 - gol_im_yjp144) / gol_im_yjp144) * 100; end
if(gol_yjp145[63:32] == 0) begin err_re_per145 = (dut_re_yjp145 - gol_re_yjp145); end else begin err_re_per145 = ((dut_re_yjp145 - gol_re_yjp145) / gol_re_yjp145) * 100; end
if(gol_yjp145[31:0] == 0) begin err_im_per145 = (dut_im_yjp145 - gol_im_yjp145); end else begin err_im_per145 = ((dut_im_yjp145 - gol_im_yjp145) / gol_im_yjp145) * 100; end
if(gol_yjp146[63:32] == 0) begin err_re_per146 = (dut_re_yjp146 - gol_re_yjp146); end else begin err_re_per146 = ((dut_re_yjp146 - gol_re_yjp146) / gol_re_yjp146) * 100; end
if(gol_yjp146[31:0] == 0) begin err_im_per146 = (dut_im_yjp146 - gol_im_yjp146); end else begin err_im_per146 = ((dut_im_yjp146 - gol_im_yjp146) / gol_im_yjp146) * 100; end
if(gol_yjp147[63:32] == 0) begin err_re_per147 = (dut_re_yjp147 - gol_re_yjp147); end else begin err_re_per147 = ((dut_re_yjp147 - gol_re_yjp147) / gol_re_yjp147) * 100; end
if(gol_yjp147[31:0] == 0) begin err_im_per147 = (dut_im_yjp147 - gol_im_yjp147); end else begin err_im_per147 = ((dut_im_yjp147 - gol_im_yjp147) / gol_im_yjp147) * 100; end
if(gol_yjp148[63:32] == 0) begin err_re_per148 = (dut_re_yjp148 - gol_re_yjp148); end else begin err_re_per148 = ((dut_re_yjp148 - gol_re_yjp148) / gol_re_yjp148) * 100; end
if(gol_yjp148[31:0] == 0) begin err_im_per148 = (dut_im_yjp148 - gol_im_yjp148); end else begin err_im_per148 = ((dut_im_yjp148 - gol_im_yjp148) / gol_im_yjp148) * 100; end
if(gol_yjp149[63:32] == 0) begin err_re_per149 = (dut_re_yjp149 - gol_re_yjp149); end else begin err_re_per149 = ((dut_re_yjp149 - gol_re_yjp149) / gol_re_yjp149) * 100; end
if(gol_yjp149[31:0] == 0) begin err_im_per149 = (dut_im_yjp149 - gol_im_yjp149); end else begin err_im_per149 = ((dut_im_yjp149 - gol_im_yjp149) / gol_im_yjp149) * 100; end
if(gol_yjp150[63:32] == 0) begin err_re_per150 = (dut_re_yjp150 - gol_re_yjp150); end else begin err_re_per150 = ((dut_re_yjp150 - gol_re_yjp150) / gol_re_yjp150) * 100; end
if(gol_yjp150[31:0] == 0) begin err_im_per150 = (dut_im_yjp150 - gol_im_yjp150); end else begin err_im_per150 = ((dut_im_yjp150 - gol_im_yjp150) / gol_im_yjp150) * 100; end
if(gol_yjp151[63:32] == 0) begin err_re_per151 = (dut_re_yjp151 - gol_re_yjp151); end else begin err_re_per151 = ((dut_re_yjp151 - gol_re_yjp151) / gol_re_yjp151) * 100; end
if(gol_yjp151[31:0] == 0) begin err_im_per151 = (dut_im_yjp151 - gol_im_yjp151); end else begin err_im_per151 = ((dut_im_yjp151 - gol_im_yjp151) / gol_im_yjp151) * 100; end
if(gol_yjp152[63:32] == 0) begin err_re_per152 = (dut_re_yjp152 - gol_re_yjp152); end else begin err_re_per152 = ((dut_re_yjp152 - gol_re_yjp152) / gol_re_yjp152) * 100; end
if(gol_yjp152[31:0] == 0) begin err_im_per152 = (dut_im_yjp152 - gol_im_yjp152); end else begin err_im_per152 = ((dut_im_yjp152 - gol_im_yjp152) / gol_im_yjp152) * 100; end
if(gol_yjp153[63:32] == 0) begin err_re_per153 = (dut_re_yjp153 - gol_re_yjp153); end else begin err_re_per153 = ((dut_re_yjp153 - gol_re_yjp153) / gol_re_yjp153) * 100; end
if(gol_yjp153[31:0] == 0) begin err_im_per153 = (dut_im_yjp153 - gol_im_yjp153); end else begin err_im_per153 = ((dut_im_yjp153 - gol_im_yjp153) / gol_im_yjp153) * 100; end
if(gol_yjp154[63:32] == 0) begin err_re_per154 = (dut_re_yjp154 - gol_re_yjp154); end else begin err_re_per154 = ((dut_re_yjp154 - gol_re_yjp154) / gol_re_yjp154) * 100; end
if(gol_yjp154[31:0] == 0) begin err_im_per154 = (dut_im_yjp154 - gol_im_yjp154); end else begin err_im_per154 = ((dut_im_yjp154 - gol_im_yjp154) / gol_im_yjp154) * 100; end
if(gol_yjp155[63:32] == 0) begin err_re_per155 = (dut_re_yjp155 - gol_re_yjp155); end else begin err_re_per155 = ((dut_re_yjp155 - gol_re_yjp155) / gol_re_yjp155) * 100; end
if(gol_yjp155[31:0] == 0) begin err_im_per155 = (dut_im_yjp155 - gol_im_yjp155); end else begin err_im_per155 = ((dut_im_yjp155 - gol_im_yjp155) / gol_im_yjp155) * 100; end
if(gol_yjp156[63:32] == 0) begin err_re_per156 = (dut_re_yjp156 - gol_re_yjp156); end else begin err_re_per156 = ((dut_re_yjp156 - gol_re_yjp156) / gol_re_yjp156) * 100; end
if(gol_yjp156[31:0] == 0) begin err_im_per156 = (dut_im_yjp156 - gol_im_yjp156); end else begin err_im_per156 = ((dut_im_yjp156 - gol_im_yjp156) / gol_im_yjp156) * 100; end
if(gol_yjp157[63:32] == 0) begin err_re_per157 = (dut_re_yjp157 - gol_re_yjp157); end else begin err_re_per157 = ((dut_re_yjp157 - gol_re_yjp157) / gol_re_yjp157) * 100; end
if(gol_yjp157[31:0] == 0) begin err_im_per157 = (dut_im_yjp157 - gol_im_yjp157); end else begin err_im_per157 = ((dut_im_yjp157 - gol_im_yjp157) / gol_im_yjp157) * 100; end
if(gol_yjp158[63:32] == 0) begin err_re_per158 = (dut_re_yjp158 - gol_re_yjp158); end else begin err_re_per158 = ((dut_re_yjp158 - gol_re_yjp158) / gol_re_yjp158) * 100; end
if(gol_yjp158[31:0] == 0) begin err_im_per158 = (dut_im_yjp158 - gol_im_yjp158); end else begin err_im_per158 = ((dut_im_yjp158 - gol_im_yjp158) / gol_im_yjp158) * 100; end
if(gol_yjp159[63:32] == 0) begin err_re_per159 = (dut_re_yjp159 - gol_re_yjp159); end else begin err_re_per159 = ((dut_re_yjp159 - gol_re_yjp159) / gol_re_yjp159) * 100; end
if(gol_yjp159[31:0] == 0) begin err_im_per159 = (dut_im_yjp159 - gol_im_yjp159); end else begin err_im_per159 = ((dut_im_yjp159 - gol_im_yjp159) / gol_im_yjp159) * 100; end
if(gol_yjp160[63:32] == 0) begin err_re_per160 = (dut_re_yjp160 - gol_re_yjp160); end else begin err_re_per160 = ((dut_re_yjp160 - gol_re_yjp160) / gol_re_yjp160) * 100; end
if(gol_yjp160[31:0] == 0) begin err_im_per160 = (dut_im_yjp160 - gol_im_yjp160); end else begin err_im_per160 = ((dut_im_yjp160 - gol_im_yjp160) / gol_im_yjp160) * 100; end
if(gol_yjp161[63:32] == 0) begin err_re_per161 = (dut_re_yjp161 - gol_re_yjp161); end else begin err_re_per161 = ((dut_re_yjp161 - gol_re_yjp161) / gol_re_yjp161) * 100; end
if(gol_yjp161[31:0] == 0) begin err_im_per161 = (dut_im_yjp161 - gol_im_yjp161); end else begin err_im_per161 = ((dut_im_yjp161 - gol_im_yjp161) / gol_im_yjp161) * 100; end
if(gol_yjp162[63:32] == 0) begin err_re_per162 = (dut_re_yjp162 - gol_re_yjp162); end else begin err_re_per162 = ((dut_re_yjp162 - gol_re_yjp162) / gol_re_yjp162) * 100; end
if(gol_yjp162[31:0] == 0) begin err_im_per162 = (dut_im_yjp162 - gol_im_yjp162); end else begin err_im_per162 = ((dut_im_yjp162 - gol_im_yjp162) / gol_im_yjp162) * 100; end
if(gol_yjp163[63:32] == 0) begin err_re_per163 = (dut_re_yjp163 - gol_re_yjp163); end else begin err_re_per163 = ((dut_re_yjp163 - gol_re_yjp163) / gol_re_yjp163) * 100; end
if(gol_yjp163[31:0] == 0) begin err_im_per163 = (dut_im_yjp163 - gol_im_yjp163); end else begin err_im_per163 = ((dut_im_yjp163 - gol_im_yjp163) / gol_im_yjp163) * 100; end
if(gol_yjp164[63:32] == 0) begin err_re_per164 = (dut_re_yjp164 - gol_re_yjp164); end else begin err_re_per164 = ((dut_re_yjp164 - gol_re_yjp164) / gol_re_yjp164) * 100; end
if(gol_yjp164[31:0] == 0) begin err_im_per164 = (dut_im_yjp164 - gol_im_yjp164); end else begin err_im_per164 = ((dut_im_yjp164 - gol_im_yjp164) / gol_im_yjp164) * 100; end
if(gol_yjp165[63:32] == 0) begin err_re_per165 = (dut_re_yjp165 - gol_re_yjp165); end else begin err_re_per165 = ((dut_re_yjp165 - gol_re_yjp165) / gol_re_yjp165) * 100; end
if(gol_yjp165[31:0] == 0) begin err_im_per165 = (dut_im_yjp165 - gol_im_yjp165); end else begin err_im_per165 = ((dut_im_yjp165 - gol_im_yjp165) / gol_im_yjp165) * 100; end
if(gol_yjp166[63:32] == 0) begin err_re_per166 = (dut_re_yjp166 - gol_re_yjp166); end else begin err_re_per166 = ((dut_re_yjp166 - gol_re_yjp166) / gol_re_yjp166) * 100; end
if(gol_yjp166[31:0] == 0) begin err_im_per166 = (dut_im_yjp166 - gol_im_yjp166); end else begin err_im_per166 = ((dut_im_yjp166 - gol_im_yjp166) / gol_im_yjp166) * 100; end
if(gol_yjp167[63:32] == 0) begin err_re_per167 = (dut_re_yjp167 - gol_re_yjp167); end else begin err_re_per167 = ((dut_re_yjp167 - gol_re_yjp167) / gol_re_yjp167) * 100; end
if(gol_yjp167[31:0] == 0) begin err_im_per167 = (dut_im_yjp167 - gol_im_yjp167); end else begin err_im_per167 = ((dut_im_yjp167 - gol_im_yjp167) / gol_im_yjp167) * 100; end
if(gol_yjp168[63:32] == 0) begin err_re_per168 = (dut_re_yjp168 - gol_re_yjp168); end else begin err_re_per168 = ((dut_re_yjp168 - gol_re_yjp168) / gol_re_yjp168) * 100; end
if(gol_yjp168[31:0] == 0) begin err_im_per168 = (dut_im_yjp168 - gol_im_yjp168); end else begin err_im_per168 = ((dut_im_yjp168 - gol_im_yjp168) / gol_im_yjp168) * 100; end
if(gol_yjp169[63:32] == 0) begin err_re_per169 = (dut_re_yjp169 - gol_re_yjp169); end else begin err_re_per169 = ((dut_re_yjp169 - gol_re_yjp169) / gol_re_yjp169) * 100; end
if(gol_yjp169[31:0] == 0) begin err_im_per169 = (dut_im_yjp169 - gol_im_yjp169); end else begin err_im_per169 = ((dut_im_yjp169 - gol_im_yjp169) / gol_im_yjp169) * 100; end
if(gol_yjp170[63:32] == 0) begin err_re_per170 = (dut_re_yjp170 - gol_re_yjp170); end else begin err_re_per170 = ((dut_re_yjp170 - gol_re_yjp170) / gol_re_yjp170) * 100; end
if(gol_yjp170[31:0] == 0) begin err_im_per170 = (dut_im_yjp170 - gol_im_yjp170); end else begin err_im_per170 = ((dut_im_yjp170 - gol_im_yjp170) / gol_im_yjp170) * 100; end
if(gol_yjp171[63:32] == 0) begin err_re_per171 = (dut_re_yjp171 - gol_re_yjp171); end else begin err_re_per171 = ((dut_re_yjp171 - gol_re_yjp171) / gol_re_yjp171) * 100; end
if(gol_yjp171[31:0] == 0) begin err_im_per171 = (dut_im_yjp171 - gol_im_yjp171); end else begin err_im_per171 = ((dut_im_yjp171 - gol_im_yjp171) / gol_im_yjp171) * 100; end
if(gol_yjp172[63:32] == 0) begin err_re_per172 = (dut_re_yjp172 - gol_re_yjp172); end else begin err_re_per172 = ((dut_re_yjp172 - gol_re_yjp172) / gol_re_yjp172) * 100; end
if(gol_yjp172[31:0] == 0) begin err_im_per172 = (dut_im_yjp172 - gol_im_yjp172); end else begin err_im_per172 = ((dut_im_yjp172 - gol_im_yjp172) / gol_im_yjp172) * 100; end
if(gol_yjp173[63:32] == 0) begin err_re_per173 = (dut_re_yjp173 - gol_re_yjp173); end else begin err_re_per173 = ((dut_re_yjp173 - gol_re_yjp173) / gol_re_yjp173) * 100; end
if(gol_yjp173[31:0] == 0) begin err_im_per173 = (dut_im_yjp173 - gol_im_yjp173); end else begin err_im_per173 = ((dut_im_yjp173 - gol_im_yjp173) / gol_im_yjp173) * 100; end
if(gol_yjp174[63:32] == 0) begin err_re_per174 = (dut_re_yjp174 - gol_re_yjp174); end else begin err_re_per174 = ((dut_re_yjp174 - gol_re_yjp174) / gol_re_yjp174) * 100; end
if(gol_yjp174[31:0] == 0) begin err_im_per174 = (dut_im_yjp174 - gol_im_yjp174); end else begin err_im_per174 = ((dut_im_yjp174 - gol_im_yjp174) / gol_im_yjp174) * 100; end
if(gol_yjp175[63:32] == 0) begin err_re_per175 = (dut_re_yjp175 - gol_re_yjp175); end else begin err_re_per175 = ((dut_re_yjp175 - gol_re_yjp175) / gol_re_yjp175) * 100; end
if(gol_yjp175[31:0] == 0) begin err_im_per175 = (dut_im_yjp175 - gol_im_yjp175); end else begin err_im_per175 = ((dut_im_yjp175 - gol_im_yjp175) / gol_im_yjp175) * 100; end
if(gol_yjp176[63:32] == 0) begin err_re_per176 = (dut_re_yjp176 - gol_re_yjp176); end else begin err_re_per176 = ((dut_re_yjp176 - gol_re_yjp176) / gol_re_yjp176) * 100; end
if(gol_yjp176[31:0] == 0) begin err_im_per176 = (dut_im_yjp176 - gol_im_yjp176); end else begin err_im_per176 = ((dut_im_yjp176 - gol_im_yjp176) / gol_im_yjp176) * 100; end
if(gol_yjp177[63:32] == 0) begin err_re_per177 = (dut_re_yjp177 - gol_re_yjp177); end else begin err_re_per177 = ((dut_re_yjp177 - gol_re_yjp177) / gol_re_yjp177) * 100; end
if(gol_yjp177[31:0] == 0) begin err_im_per177 = (dut_im_yjp177 - gol_im_yjp177); end else begin err_im_per177 = ((dut_im_yjp177 - gol_im_yjp177) / gol_im_yjp177) * 100; end
if(gol_yjp178[63:32] == 0) begin err_re_per178 = (dut_re_yjp178 - gol_re_yjp178); end else begin err_re_per178 = ((dut_re_yjp178 - gol_re_yjp178) / gol_re_yjp178) * 100; end
if(gol_yjp178[31:0] == 0) begin err_im_per178 = (dut_im_yjp178 - gol_im_yjp178); end else begin err_im_per178 = ((dut_im_yjp178 - gol_im_yjp178) / gol_im_yjp178) * 100; end
if(gol_yjp179[63:32] == 0) begin err_re_per179 = (dut_re_yjp179 - gol_re_yjp179); end else begin err_re_per179 = ((dut_re_yjp179 - gol_re_yjp179) / gol_re_yjp179) * 100; end
if(gol_yjp179[31:0] == 0) begin err_im_per179 = (dut_im_yjp179 - gol_im_yjp179); end else begin err_im_per179 = ((dut_im_yjp179 - gol_im_yjp179) / gol_im_yjp179) * 100; end
if(gol_yjp180[63:32] == 0) begin err_re_per180 = (dut_re_yjp180 - gol_re_yjp180); end else begin err_re_per180 = ((dut_re_yjp180 - gol_re_yjp180) / gol_re_yjp180) * 100; end
if(gol_yjp180[31:0] == 0) begin err_im_per180 = (dut_im_yjp180 - gol_im_yjp180); end else begin err_im_per180 = ((dut_im_yjp180 - gol_im_yjp180) / gol_im_yjp180) * 100; end
if(gol_yjp181[63:32] == 0) begin err_re_per181 = (dut_re_yjp181 - gol_re_yjp181); end else begin err_re_per181 = ((dut_re_yjp181 - gol_re_yjp181) / gol_re_yjp181) * 100; end
if(gol_yjp181[31:0] == 0) begin err_im_per181 = (dut_im_yjp181 - gol_im_yjp181); end else begin err_im_per181 = ((dut_im_yjp181 - gol_im_yjp181) / gol_im_yjp181) * 100; end
if(gol_yjp182[63:32] == 0) begin err_re_per182 = (dut_re_yjp182 - gol_re_yjp182); end else begin err_re_per182 = ((dut_re_yjp182 - gol_re_yjp182) / gol_re_yjp182) * 100; end
if(gol_yjp182[31:0] == 0) begin err_im_per182 = (dut_im_yjp182 - gol_im_yjp182); end else begin err_im_per182 = ((dut_im_yjp182 - gol_im_yjp182) / gol_im_yjp182) * 100; end
if(gol_yjp183[63:32] == 0) begin err_re_per183 = (dut_re_yjp183 - gol_re_yjp183); end else begin err_re_per183 = ((dut_re_yjp183 - gol_re_yjp183) / gol_re_yjp183) * 100; end
if(gol_yjp183[31:0] == 0) begin err_im_per183 = (dut_im_yjp183 - gol_im_yjp183); end else begin err_im_per183 = ((dut_im_yjp183 - gol_im_yjp183) / gol_im_yjp183) * 100; end
if(gol_yjp184[63:32] == 0) begin err_re_per184 = (dut_re_yjp184 - gol_re_yjp184); end else begin err_re_per184 = ((dut_re_yjp184 - gol_re_yjp184) / gol_re_yjp184) * 100; end
if(gol_yjp184[31:0] == 0) begin err_im_per184 = (dut_im_yjp184 - gol_im_yjp184); end else begin err_im_per184 = ((dut_im_yjp184 - gol_im_yjp184) / gol_im_yjp184) * 100; end
if(gol_yjp185[63:32] == 0) begin err_re_per185 = (dut_re_yjp185 - gol_re_yjp185); end else begin err_re_per185 = ((dut_re_yjp185 - gol_re_yjp185) / gol_re_yjp185) * 100; end
if(gol_yjp185[31:0] == 0) begin err_im_per185 = (dut_im_yjp185 - gol_im_yjp185); end else begin err_im_per185 = ((dut_im_yjp185 - gol_im_yjp185) / gol_im_yjp185) * 100; end
if(gol_yjp186[63:32] == 0) begin err_re_per186 = (dut_re_yjp186 - gol_re_yjp186); end else begin err_re_per186 = ((dut_re_yjp186 - gol_re_yjp186) / gol_re_yjp186) * 100; end
if(gol_yjp186[31:0] == 0) begin err_im_per186 = (dut_im_yjp186 - gol_im_yjp186); end else begin err_im_per186 = ((dut_im_yjp186 - gol_im_yjp186) / gol_im_yjp186) * 100; end
if(gol_yjp187[63:32] == 0) begin err_re_per187 = (dut_re_yjp187 - gol_re_yjp187); end else begin err_re_per187 = ((dut_re_yjp187 - gol_re_yjp187) / gol_re_yjp187) * 100; end
if(gol_yjp187[31:0] == 0) begin err_im_per187 = (dut_im_yjp187 - gol_im_yjp187); end else begin err_im_per187 = ((dut_im_yjp187 - gol_im_yjp187) / gol_im_yjp187) * 100; end
if(gol_yjp188[63:32] == 0) begin err_re_per188 = (dut_re_yjp188 - gol_re_yjp188); end else begin err_re_per188 = ((dut_re_yjp188 - gol_re_yjp188) / gol_re_yjp188) * 100; end
if(gol_yjp188[31:0] == 0) begin err_im_per188 = (dut_im_yjp188 - gol_im_yjp188); end else begin err_im_per188 = ((dut_im_yjp188 - gol_im_yjp188) / gol_im_yjp188) * 100; end
if(gol_yjp189[63:32] == 0) begin err_re_per189 = (dut_re_yjp189 - gol_re_yjp189); end else begin err_re_per189 = ((dut_re_yjp189 - gol_re_yjp189) / gol_re_yjp189) * 100; end
if(gol_yjp189[31:0] == 0) begin err_im_per189 = (dut_im_yjp189 - gol_im_yjp189); end else begin err_im_per189 = ((dut_im_yjp189 - gol_im_yjp189) / gol_im_yjp189) * 100; end
if(gol_yjp190[63:32] == 0) begin err_re_per190 = (dut_re_yjp190 - gol_re_yjp190); end else begin err_re_per190 = ((dut_re_yjp190 - gol_re_yjp190) / gol_re_yjp190) * 100; end
if(gol_yjp190[31:0] == 0) begin err_im_per190 = (dut_im_yjp190 - gol_im_yjp190); end else begin err_im_per190 = ((dut_im_yjp190 - gol_im_yjp190) / gol_im_yjp190) * 100; end
if(gol_yjp191[63:32] == 0) begin err_re_per191 = (dut_re_yjp191 - gol_re_yjp191); end else begin err_re_per191 = ((dut_re_yjp191 - gol_re_yjp191) / gol_re_yjp191) * 100; end
if(gol_yjp191[31:0] == 0) begin err_im_per191 = (dut_im_yjp191 - gol_im_yjp191); end else begin err_im_per191 = ((dut_im_yjp191 - gol_im_yjp191) / gol_im_yjp191) * 100; end
if(gol_yjp192[63:32] == 0) begin err_re_per192 = (dut_re_yjp192 - gol_re_yjp192); end else begin err_re_per192 = ((dut_re_yjp192 - gol_re_yjp192) / gol_re_yjp192) * 100; end
if(gol_yjp192[31:0] == 0) begin err_im_per192 = (dut_im_yjp192 - gol_im_yjp192); end else begin err_im_per192 = ((dut_im_yjp192 - gol_im_yjp192) / gol_im_yjp192) * 100; end
if(gol_yjp193[63:32] == 0) begin err_re_per193 = (dut_re_yjp193 - gol_re_yjp193); end else begin err_re_per193 = ((dut_re_yjp193 - gol_re_yjp193) / gol_re_yjp193) * 100; end
if(gol_yjp193[31:0] == 0) begin err_im_per193 = (dut_im_yjp193 - gol_im_yjp193); end else begin err_im_per193 = ((dut_im_yjp193 - gol_im_yjp193) / gol_im_yjp193) * 100; end
if(gol_yjp194[63:32] == 0) begin err_re_per194 = (dut_re_yjp194 - gol_re_yjp194); end else begin err_re_per194 = ((dut_re_yjp194 - gol_re_yjp194) / gol_re_yjp194) * 100; end
if(gol_yjp194[31:0] == 0) begin err_im_per194 = (dut_im_yjp194 - gol_im_yjp194); end else begin err_im_per194 = ((dut_im_yjp194 - gol_im_yjp194) / gol_im_yjp194) * 100; end
if(gol_yjp195[63:32] == 0) begin err_re_per195 = (dut_re_yjp195 - gol_re_yjp195); end else begin err_re_per195 = ((dut_re_yjp195 - gol_re_yjp195) / gol_re_yjp195) * 100; end
if(gol_yjp195[31:0] == 0) begin err_im_per195 = (dut_im_yjp195 - gol_im_yjp195); end else begin err_im_per195 = ((dut_im_yjp195 - gol_im_yjp195) / gol_im_yjp195) * 100; end
if(gol_yjp196[63:32] == 0) begin err_re_per196 = (dut_re_yjp196 - gol_re_yjp196); end else begin err_re_per196 = ((dut_re_yjp196 - gol_re_yjp196) / gol_re_yjp196) * 100; end
if(gol_yjp196[31:0] == 0) begin err_im_per196 = (dut_im_yjp196 - gol_im_yjp196); end else begin err_im_per196 = ((dut_im_yjp196 - gol_im_yjp196) / gol_im_yjp196) * 100; end
if(gol_yjp197[63:32] == 0) begin err_re_per197 = (dut_re_yjp197 - gol_re_yjp197); end else begin err_re_per197 = ((dut_re_yjp197 - gol_re_yjp197) / gol_re_yjp197) * 100; end
if(gol_yjp197[31:0] == 0) begin err_im_per197 = (dut_im_yjp197 - gol_im_yjp197); end else begin err_im_per197 = ((dut_im_yjp197 - gol_im_yjp197) / gol_im_yjp197) * 100; end
if(gol_yjp198[63:32] == 0) begin err_re_per198 = (dut_re_yjp198 - gol_re_yjp198); end else begin err_re_per198 = ((dut_re_yjp198 - gol_re_yjp198) / gol_re_yjp198) * 100; end
if(gol_yjp198[31:0] == 0) begin err_im_per198 = (dut_im_yjp198 - gol_im_yjp198); end else begin err_im_per198 = ((dut_im_yjp198 - gol_im_yjp198) / gol_im_yjp198) * 100; end
if(gol_yjp199[63:32] == 0) begin err_re_per199 = (dut_re_yjp199 - gol_re_yjp199); end else begin err_re_per199 = ((dut_re_yjp199 - gol_re_yjp199) / gol_re_yjp199) * 100; end
if(gol_yjp199[31:0] == 0) begin err_im_per199 = (dut_im_yjp199 - gol_im_yjp199); end else begin err_im_per199 = ((dut_im_yjp199 - gol_im_yjp199) / gol_im_yjp199) * 100; end
if(gol_yjp200[63:32] == 0) begin err_re_per200 = (dut_re_yjp200 - gol_re_yjp200); end else begin err_re_per200 = ((dut_re_yjp200 - gol_re_yjp200) / gol_re_yjp200) * 100; end
if(gol_yjp200[31:0] == 0) begin err_im_per200 = (dut_im_yjp200 - gol_im_yjp200); end else begin err_im_per200 = ((dut_im_yjp200 - gol_im_yjp200) / gol_im_yjp200) * 100; end
if(gol_yjp201[63:32] == 0) begin err_re_per201 = (dut_re_yjp201 - gol_re_yjp201); end else begin err_re_per201 = ((dut_re_yjp201 - gol_re_yjp201) / gol_re_yjp201) * 100; end
if(gol_yjp201[31:0] == 0) begin err_im_per201 = (dut_im_yjp201 - gol_im_yjp201); end else begin err_im_per201 = ((dut_im_yjp201 - gol_im_yjp201) / gol_im_yjp201) * 100; end
if(gol_yjp202[63:32] == 0) begin err_re_per202 = (dut_re_yjp202 - gol_re_yjp202); end else begin err_re_per202 = ((dut_re_yjp202 - gol_re_yjp202) / gol_re_yjp202) * 100; end
if(gol_yjp202[31:0] == 0) begin err_im_per202 = (dut_im_yjp202 - gol_im_yjp202); end else begin err_im_per202 = ((dut_im_yjp202 - gol_im_yjp202) / gol_im_yjp202) * 100; end
if(gol_yjp203[63:32] == 0) begin err_re_per203 = (dut_re_yjp203 - gol_re_yjp203); end else begin err_re_per203 = ((dut_re_yjp203 - gol_re_yjp203) / gol_re_yjp203) * 100; end
if(gol_yjp203[31:0] == 0) begin err_im_per203 = (dut_im_yjp203 - gol_im_yjp203); end else begin err_im_per203 = ((dut_im_yjp203 - gol_im_yjp203) / gol_im_yjp203) * 100; end
if(gol_yjp204[63:32] == 0) begin err_re_per204 = (dut_re_yjp204 - gol_re_yjp204); end else begin err_re_per204 = ((dut_re_yjp204 - gol_re_yjp204) / gol_re_yjp204) * 100; end
if(gol_yjp204[31:0] == 0) begin err_im_per204 = (dut_im_yjp204 - gol_im_yjp204); end else begin err_im_per204 = ((dut_im_yjp204 - gol_im_yjp204) / gol_im_yjp204) * 100; end
if(gol_yjp205[63:32] == 0) begin err_re_per205 = (dut_re_yjp205 - gol_re_yjp205); end else begin err_re_per205 = ((dut_re_yjp205 - gol_re_yjp205) / gol_re_yjp205) * 100; end
if(gol_yjp205[31:0] == 0) begin err_im_per205 = (dut_im_yjp205 - gol_im_yjp205); end else begin err_im_per205 = ((dut_im_yjp205 - gol_im_yjp205) / gol_im_yjp205) * 100; end
if(gol_yjp206[63:32] == 0) begin err_re_per206 = (dut_re_yjp206 - gol_re_yjp206); end else begin err_re_per206 = ((dut_re_yjp206 - gol_re_yjp206) / gol_re_yjp206) * 100; end
if(gol_yjp206[31:0] == 0) begin err_im_per206 = (dut_im_yjp206 - gol_im_yjp206); end else begin err_im_per206 = ((dut_im_yjp206 - gol_im_yjp206) / gol_im_yjp206) * 100; end
if(gol_yjp207[63:32] == 0) begin err_re_per207 = (dut_re_yjp207 - gol_re_yjp207); end else begin err_re_per207 = ((dut_re_yjp207 - gol_re_yjp207) / gol_re_yjp207) * 100; end
if(gol_yjp207[31:0] == 0) begin err_im_per207 = (dut_im_yjp207 - gol_im_yjp207); end else begin err_im_per207 = ((dut_im_yjp207 - gol_im_yjp207) / gol_im_yjp207) * 100; end
if(gol_yjp208[63:32] == 0) begin err_re_per208 = (dut_re_yjp208 - gol_re_yjp208); end else begin err_re_per208 = ((dut_re_yjp208 - gol_re_yjp208) / gol_re_yjp208) * 100; end
if(gol_yjp208[31:0] == 0) begin err_im_per208 = (dut_im_yjp208 - gol_im_yjp208); end else begin err_im_per208 = ((dut_im_yjp208 - gol_im_yjp208) / gol_im_yjp208) * 100; end
if(gol_yjp209[63:32] == 0) begin err_re_per209 = (dut_re_yjp209 - gol_re_yjp209); end else begin err_re_per209 = ((dut_re_yjp209 - gol_re_yjp209) / gol_re_yjp209) * 100; end
if(gol_yjp209[31:0] == 0) begin err_im_per209 = (dut_im_yjp209 - gol_im_yjp209); end else begin err_im_per209 = ((dut_im_yjp209 - gol_im_yjp209) / gol_im_yjp209) * 100; end
if(gol_yjp210[63:32] == 0) begin err_re_per210 = (dut_re_yjp210 - gol_re_yjp210); end else begin err_re_per210 = ((dut_re_yjp210 - gol_re_yjp210) / gol_re_yjp210) * 100; end
if(gol_yjp210[31:0] == 0) begin err_im_per210 = (dut_im_yjp210 - gol_im_yjp210); end else begin err_im_per210 = ((dut_im_yjp210 - gol_im_yjp210) / gol_im_yjp210) * 100; end
if(gol_yjp211[63:32] == 0) begin err_re_per211 = (dut_re_yjp211 - gol_re_yjp211); end else begin err_re_per211 = ((dut_re_yjp211 - gol_re_yjp211) / gol_re_yjp211) * 100; end
if(gol_yjp211[31:0] == 0) begin err_im_per211 = (dut_im_yjp211 - gol_im_yjp211); end else begin err_im_per211 = ((dut_im_yjp211 - gol_im_yjp211) / gol_im_yjp211) * 100; end
if(gol_yjp212[63:32] == 0) begin err_re_per212 = (dut_re_yjp212 - gol_re_yjp212); end else begin err_re_per212 = ((dut_re_yjp212 - gol_re_yjp212) / gol_re_yjp212) * 100; end
if(gol_yjp212[31:0] == 0) begin err_im_per212 = (dut_im_yjp212 - gol_im_yjp212); end else begin err_im_per212 = ((dut_im_yjp212 - gol_im_yjp212) / gol_im_yjp212) * 100; end
if(gol_yjp213[63:32] == 0) begin err_re_per213 = (dut_re_yjp213 - gol_re_yjp213); end else begin err_re_per213 = ((dut_re_yjp213 - gol_re_yjp213) / gol_re_yjp213) * 100; end
if(gol_yjp213[31:0] == 0) begin err_im_per213 = (dut_im_yjp213 - gol_im_yjp213); end else begin err_im_per213 = ((dut_im_yjp213 - gol_im_yjp213) / gol_im_yjp213) * 100; end
if(gol_yjp214[63:32] == 0) begin err_re_per214 = (dut_re_yjp214 - gol_re_yjp214); end else begin err_re_per214 = ((dut_re_yjp214 - gol_re_yjp214) / gol_re_yjp214) * 100; end
if(gol_yjp214[31:0] == 0) begin err_im_per214 = (dut_im_yjp214 - gol_im_yjp214); end else begin err_im_per214 = ((dut_im_yjp214 - gol_im_yjp214) / gol_im_yjp214) * 100; end
if(gol_yjp215[63:32] == 0) begin err_re_per215 = (dut_re_yjp215 - gol_re_yjp215); end else begin err_re_per215 = ((dut_re_yjp215 - gol_re_yjp215) / gol_re_yjp215) * 100; end
if(gol_yjp215[31:0] == 0) begin err_im_per215 = (dut_im_yjp215 - gol_im_yjp215); end else begin err_im_per215 = ((dut_im_yjp215 - gol_im_yjp215) / gol_im_yjp215) * 100; end
if(gol_yjp216[63:32] == 0) begin err_re_per216 = (dut_re_yjp216 - gol_re_yjp216); end else begin err_re_per216 = ((dut_re_yjp216 - gol_re_yjp216) / gol_re_yjp216) * 100; end
if(gol_yjp216[31:0] == 0) begin err_im_per216 = (dut_im_yjp216 - gol_im_yjp216); end else begin err_im_per216 = ((dut_im_yjp216 - gol_im_yjp216) / gol_im_yjp216) * 100; end
if(gol_yjp217[63:32] == 0) begin err_re_per217 = (dut_re_yjp217 - gol_re_yjp217); end else begin err_re_per217 = ((dut_re_yjp217 - gol_re_yjp217) / gol_re_yjp217) * 100; end
if(gol_yjp217[31:0] == 0) begin err_im_per217 = (dut_im_yjp217 - gol_im_yjp217); end else begin err_im_per217 = ((dut_im_yjp217 - gol_im_yjp217) / gol_im_yjp217) * 100; end
if(gol_yjp218[63:32] == 0) begin err_re_per218 = (dut_re_yjp218 - gol_re_yjp218); end else begin err_re_per218 = ((dut_re_yjp218 - gol_re_yjp218) / gol_re_yjp218) * 100; end
if(gol_yjp218[31:0] == 0) begin err_im_per218 = (dut_im_yjp218 - gol_im_yjp218); end else begin err_im_per218 = ((dut_im_yjp218 - gol_im_yjp218) / gol_im_yjp218) * 100; end
if(gol_yjp219[63:32] == 0) begin err_re_per219 = (dut_re_yjp219 - gol_re_yjp219); end else begin err_re_per219 = ((dut_re_yjp219 - gol_re_yjp219) / gol_re_yjp219) * 100; end
if(gol_yjp219[31:0] == 0) begin err_im_per219 = (dut_im_yjp219 - gol_im_yjp219); end else begin err_im_per219 = ((dut_im_yjp219 - gol_im_yjp219) / gol_im_yjp219) * 100; end
if(gol_yjp220[63:32] == 0) begin err_re_per220 = (dut_re_yjp220 - gol_re_yjp220); end else begin err_re_per220 = ((dut_re_yjp220 - gol_re_yjp220) / gol_re_yjp220) * 100; end
if(gol_yjp220[31:0] == 0) begin err_im_per220 = (dut_im_yjp220 - gol_im_yjp220); end else begin err_im_per220 = ((dut_im_yjp220 - gol_im_yjp220) / gol_im_yjp220) * 100; end
if(gol_yjp221[63:32] == 0) begin err_re_per221 = (dut_re_yjp221 - gol_re_yjp221); end else begin err_re_per221 = ((dut_re_yjp221 - gol_re_yjp221) / gol_re_yjp221) * 100; end
if(gol_yjp221[31:0] == 0) begin err_im_per221 = (dut_im_yjp221 - gol_im_yjp221); end else begin err_im_per221 = ((dut_im_yjp221 - gol_im_yjp221) / gol_im_yjp221) * 100; end
if(gol_yjp222[63:32] == 0) begin err_re_per222 = (dut_re_yjp222 - gol_re_yjp222); end else begin err_re_per222 = ((dut_re_yjp222 - gol_re_yjp222) / gol_re_yjp222) * 100; end
if(gol_yjp222[31:0] == 0) begin err_im_per222 = (dut_im_yjp222 - gol_im_yjp222); end else begin err_im_per222 = ((dut_im_yjp222 - gol_im_yjp222) / gol_im_yjp222) * 100; end
if(gol_yjp223[63:32] == 0) begin err_re_per223 = (dut_re_yjp223 - gol_re_yjp223); end else begin err_re_per223 = ((dut_re_yjp223 - gol_re_yjp223) / gol_re_yjp223) * 100; end
if(gol_yjp223[31:0] == 0) begin err_im_per223 = (dut_im_yjp223 - gol_im_yjp223); end else begin err_im_per223 = ((dut_im_yjp223 - gol_im_yjp223) / gol_im_yjp223) * 100; end
if(gol_yjp224[63:32] == 0) begin err_re_per224 = (dut_re_yjp224 - gol_re_yjp224); end else begin err_re_per224 = ((dut_re_yjp224 - gol_re_yjp224) / gol_re_yjp224) * 100; end
if(gol_yjp224[31:0] == 0) begin err_im_per224 = (dut_im_yjp224 - gol_im_yjp224); end else begin err_im_per224 = ((dut_im_yjp224 - gol_im_yjp224) / gol_im_yjp224) * 100; end
if(gol_yjp225[63:32] == 0) begin err_re_per225 = (dut_re_yjp225 - gol_re_yjp225); end else begin err_re_per225 = ((dut_re_yjp225 - gol_re_yjp225) / gol_re_yjp225) * 100; end
if(gol_yjp225[31:0] == 0) begin err_im_per225 = (dut_im_yjp225 - gol_im_yjp225); end else begin err_im_per225 = ((dut_im_yjp225 - gol_im_yjp225) / gol_im_yjp225) * 100; end
if(gol_yjp226[63:32] == 0) begin err_re_per226 = (dut_re_yjp226 - gol_re_yjp226); end else begin err_re_per226 = ((dut_re_yjp226 - gol_re_yjp226) / gol_re_yjp226) * 100; end
if(gol_yjp226[31:0] == 0) begin err_im_per226 = (dut_im_yjp226 - gol_im_yjp226); end else begin err_im_per226 = ((dut_im_yjp226 - gol_im_yjp226) / gol_im_yjp226) * 100; end
if(gol_yjp227[63:32] == 0) begin err_re_per227 = (dut_re_yjp227 - gol_re_yjp227); end else begin err_re_per227 = ((dut_re_yjp227 - gol_re_yjp227) / gol_re_yjp227) * 100; end
if(gol_yjp227[31:0] == 0) begin err_im_per227 = (dut_im_yjp227 - gol_im_yjp227); end else begin err_im_per227 = ((dut_im_yjp227 - gol_im_yjp227) / gol_im_yjp227) * 100; end
if(gol_yjp228[63:32] == 0) begin err_re_per228 = (dut_re_yjp228 - gol_re_yjp228); end else begin err_re_per228 = ((dut_re_yjp228 - gol_re_yjp228) / gol_re_yjp228) * 100; end
if(gol_yjp228[31:0] == 0) begin err_im_per228 = (dut_im_yjp228 - gol_im_yjp228); end else begin err_im_per228 = ((dut_im_yjp228 - gol_im_yjp228) / gol_im_yjp228) * 100; end
if(gol_yjp229[63:32] == 0) begin err_re_per229 = (dut_re_yjp229 - gol_re_yjp229); end else begin err_re_per229 = ((dut_re_yjp229 - gol_re_yjp229) / gol_re_yjp229) * 100; end
if(gol_yjp229[31:0] == 0) begin err_im_per229 = (dut_im_yjp229 - gol_im_yjp229); end else begin err_im_per229 = ((dut_im_yjp229 - gol_im_yjp229) / gol_im_yjp229) * 100; end
if(gol_yjp230[63:32] == 0) begin err_re_per230 = (dut_re_yjp230 - gol_re_yjp230); end else begin err_re_per230 = ((dut_re_yjp230 - gol_re_yjp230) / gol_re_yjp230) * 100; end
if(gol_yjp230[31:0] == 0) begin err_im_per230 = (dut_im_yjp230 - gol_im_yjp230); end else begin err_im_per230 = ((dut_im_yjp230 - gol_im_yjp230) / gol_im_yjp230) * 100; end
if(gol_yjp231[63:32] == 0) begin err_re_per231 = (dut_re_yjp231 - gol_re_yjp231); end else begin err_re_per231 = ((dut_re_yjp231 - gol_re_yjp231) / gol_re_yjp231) * 100; end
if(gol_yjp231[31:0] == 0) begin err_im_per231 = (dut_im_yjp231 - gol_im_yjp231); end else begin err_im_per231 = ((dut_im_yjp231 - gol_im_yjp231) / gol_im_yjp231) * 100; end
if(gol_yjp232[63:32] == 0) begin err_re_per232 = (dut_re_yjp232 - gol_re_yjp232); end else begin err_re_per232 = ((dut_re_yjp232 - gol_re_yjp232) / gol_re_yjp232) * 100; end
if(gol_yjp232[31:0] == 0) begin err_im_per232 = (dut_im_yjp232 - gol_im_yjp232); end else begin err_im_per232 = ((dut_im_yjp232 - gol_im_yjp232) / gol_im_yjp232) * 100; end
if(gol_yjp233[63:32] == 0) begin err_re_per233 = (dut_re_yjp233 - gol_re_yjp233); end else begin err_re_per233 = ((dut_re_yjp233 - gol_re_yjp233) / gol_re_yjp233) * 100; end
if(gol_yjp233[31:0] == 0) begin err_im_per233 = (dut_im_yjp233 - gol_im_yjp233); end else begin err_im_per233 = ((dut_im_yjp233 - gol_im_yjp233) / gol_im_yjp233) * 100; end
if(gol_yjp234[63:32] == 0) begin err_re_per234 = (dut_re_yjp234 - gol_re_yjp234); end else begin err_re_per234 = ((dut_re_yjp234 - gol_re_yjp234) / gol_re_yjp234) * 100; end
if(gol_yjp234[31:0] == 0) begin err_im_per234 = (dut_im_yjp234 - gol_im_yjp234); end else begin err_im_per234 = ((dut_im_yjp234 - gol_im_yjp234) / gol_im_yjp234) * 100; end
if(gol_yjp235[63:32] == 0) begin err_re_per235 = (dut_re_yjp235 - gol_re_yjp235); end else begin err_re_per235 = ((dut_re_yjp235 - gol_re_yjp235) / gol_re_yjp235) * 100; end
if(gol_yjp235[31:0] == 0) begin err_im_per235 = (dut_im_yjp235 - gol_im_yjp235); end else begin err_im_per235 = ((dut_im_yjp235 - gol_im_yjp235) / gol_im_yjp235) * 100; end
if(gol_yjp236[63:32] == 0) begin err_re_per236 = (dut_re_yjp236 - gol_re_yjp236); end else begin err_re_per236 = ((dut_re_yjp236 - gol_re_yjp236) / gol_re_yjp236) * 100; end
if(gol_yjp236[31:0] == 0) begin err_im_per236 = (dut_im_yjp236 - gol_im_yjp236); end else begin err_im_per236 = ((dut_im_yjp236 - gol_im_yjp236) / gol_im_yjp236) * 100; end
if(gol_yjp237[63:32] == 0) begin err_re_per237 = (dut_re_yjp237 - gol_re_yjp237); end else begin err_re_per237 = ((dut_re_yjp237 - gol_re_yjp237) / gol_re_yjp237) * 100; end
if(gol_yjp237[31:0] == 0) begin err_im_per237 = (dut_im_yjp237 - gol_im_yjp237); end else begin err_im_per237 = ((dut_im_yjp237 - gol_im_yjp237) / gol_im_yjp237) * 100; end
if(gol_yjp238[63:32] == 0) begin err_re_per238 = (dut_re_yjp238 - gol_re_yjp238); end else begin err_re_per238 = ((dut_re_yjp238 - gol_re_yjp238) / gol_re_yjp238) * 100; end
if(gol_yjp238[31:0] == 0) begin err_im_per238 = (dut_im_yjp238 - gol_im_yjp238); end else begin err_im_per238 = ((dut_im_yjp238 - gol_im_yjp238) / gol_im_yjp238) * 100; end
if(gol_yjp239[63:32] == 0) begin err_re_per239 = (dut_re_yjp239 - gol_re_yjp239); end else begin err_re_per239 = ((dut_re_yjp239 - gol_re_yjp239) / gol_re_yjp239) * 100; end
if(gol_yjp239[31:0] == 0) begin err_im_per239 = (dut_im_yjp239 - gol_im_yjp239); end else begin err_im_per239 = ((dut_im_yjp239 - gol_im_yjp239) / gol_im_yjp239) * 100; end
if(gol_yjp240[63:32] == 0) begin err_re_per240 = (dut_re_yjp240 - gol_re_yjp240); end else begin err_re_per240 = ((dut_re_yjp240 - gol_re_yjp240) / gol_re_yjp240) * 100; end
if(gol_yjp240[31:0] == 0) begin err_im_per240 = (dut_im_yjp240 - gol_im_yjp240); end else begin err_im_per240 = ((dut_im_yjp240 - gol_im_yjp240) / gol_im_yjp240) * 100; end
if(gol_yjp241[63:32] == 0) begin err_re_per241 = (dut_re_yjp241 - gol_re_yjp241); end else begin err_re_per241 = ((dut_re_yjp241 - gol_re_yjp241) / gol_re_yjp241) * 100; end
if(gol_yjp241[31:0] == 0) begin err_im_per241 = (dut_im_yjp241 - gol_im_yjp241); end else begin err_im_per241 = ((dut_im_yjp241 - gol_im_yjp241) / gol_im_yjp241) * 100; end
if(gol_yjp242[63:32] == 0) begin err_re_per242 = (dut_re_yjp242 - gol_re_yjp242); end else begin err_re_per242 = ((dut_re_yjp242 - gol_re_yjp242) / gol_re_yjp242) * 100; end
if(gol_yjp242[31:0] == 0) begin err_im_per242 = (dut_im_yjp242 - gol_im_yjp242); end else begin err_im_per242 = ((dut_im_yjp242 - gol_im_yjp242) / gol_im_yjp242) * 100; end
if(gol_yjp243[63:32] == 0) begin err_re_per243 = (dut_re_yjp243 - gol_re_yjp243); end else begin err_re_per243 = ((dut_re_yjp243 - gol_re_yjp243) / gol_re_yjp243) * 100; end
if(gol_yjp243[31:0] == 0) begin err_im_per243 = (dut_im_yjp243 - gol_im_yjp243); end else begin err_im_per243 = ((dut_im_yjp243 - gol_im_yjp243) / gol_im_yjp243) * 100; end
if(gol_yjp244[63:32] == 0) begin err_re_per244 = (dut_re_yjp244 - gol_re_yjp244); end else begin err_re_per244 = ((dut_re_yjp244 - gol_re_yjp244) / gol_re_yjp244) * 100; end
if(gol_yjp244[31:0] == 0) begin err_im_per244 = (dut_im_yjp244 - gol_im_yjp244); end else begin err_im_per244 = ((dut_im_yjp244 - gol_im_yjp244) / gol_im_yjp244) * 100; end
if(gol_yjp245[63:32] == 0) begin err_re_per245 = (dut_re_yjp245 - gol_re_yjp245); end else begin err_re_per245 = ((dut_re_yjp245 - gol_re_yjp245) / gol_re_yjp245) * 100; end
if(gol_yjp245[31:0] == 0) begin err_im_per245 = (dut_im_yjp245 - gol_im_yjp245); end else begin err_im_per245 = ((dut_im_yjp245 - gol_im_yjp245) / gol_im_yjp245) * 100; end
if(gol_yjp246[63:32] == 0) begin err_re_per246 = (dut_re_yjp246 - gol_re_yjp246); end else begin err_re_per246 = ((dut_re_yjp246 - gol_re_yjp246) / gol_re_yjp246) * 100; end
if(gol_yjp246[31:0] == 0) begin err_im_per246 = (dut_im_yjp246 - gol_im_yjp246); end else begin err_im_per246 = ((dut_im_yjp246 - gol_im_yjp246) / gol_im_yjp246) * 100; end
if(gol_yjp247[63:32] == 0) begin err_re_per247 = (dut_re_yjp247 - gol_re_yjp247); end else begin err_re_per247 = ((dut_re_yjp247 - gol_re_yjp247) / gol_re_yjp247) * 100; end
if(gol_yjp247[31:0] == 0) begin err_im_per247 = (dut_im_yjp247 - gol_im_yjp247); end else begin err_im_per247 = ((dut_im_yjp247 - gol_im_yjp247) / gol_im_yjp247) * 100; end
if(gol_yjp248[63:32] == 0) begin err_re_per248 = (dut_re_yjp248 - gol_re_yjp248); end else begin err_re_per248 = ((dut_re_yjp248 - gol_re_yjp248) / gol_re_yjp248) * 100; end
if(gol_yjp248[31:0] == 0) begin err_im_per248 = (dut_im_yjp248 - gol_im_yjp248); end else begin err_im_per248 = ((dut_im_yjp248 - gol_im_yjp248) / gol_im_yjp248) * 100; end
if(gol_yjp249[63:32] == 0) begin err_re_per249 = (dut_re_yjp249 - gol_re_yjp249); end else begin err_re_per249 = ((dut_re_yjp249 - gol_re_yjp249) / gol_re_yjp249) * 100; end
if(gol_yjp249[31:0] == 0) begin err_im_per249 = (dut_im_yjp249 - gol_im_yjp249); end else begin err_im_per249 = ((dut_im_yjp249 - gol_im_yjp249) / gol_im_yjp249) * 100; end
if(gol_yjp250[63:32] == 0) begin err_re_per250 = (dut_re_yjp250 - gol_re_yjp250); end else begin err_re_per250 = ((dut_re_yjp250 - gol_re_yjp250) / gol_re_yjp250) * 100; end
if(gol_yjp250[31:0] == 0) begin err_im_per250 = (dut_im_yjp250 - gol_im_yjp250); end else begin err_im_per250 = ((dut_im_yjp250 - gol_im_yjp250) / gol_im_yjp250) * 100; end
if(gol_yjp251[63:32] == 0) begin err_re_per251 = (dut_re_yjp251 - gol_re_yjp251); end else begin err_re_per251 = ((dut_re_yjp251 - gol_re_yjp251) / gol_re_yjp251) * 100; end
if(gol_yjp251[31:0] == 0) begin err_im_per251 = (dut_im_yjp251 - gol_im_yjp251); end else begin err_im_per251 = ((dut_im_yjp251 - gol_im_yjp251) / gol_im_yjp251) * 100; end
if(gol_yjp252[63:32] == 0) begin err_re_per252 = (dut_re_yjp252 - gol_re_yjp252); end else begin err_re_per252 = ((dut_re_yjp252 - gol_re_yjp252) / gol_re_yjp252) * 100; end
if(gol_yjp252[31:0] == 0) begin err_im_per252 = (dut_im_yjp252 - gol_im_yjp252); end else begin err_im_per252 = ((dut_im_yjp252 - gol_im_yjp252) / gol_im_yjp252) * 100; end
if(gol_yjp253[63:32] == 0) begin err_re_per253 = (dut_re_yjp253 - gol_re_yjp253); end else begin err_re_per253 = ((dut_re_yjp253 - gol_re_yjp253) / gol_re_yjp253) * 100; end
if(gol_yjp253[31:0] == 0) begin err_im_per253 = (dut_im_yjp253 - gol_im_yjp253); end else begin err_im_per253 = ((dut_im_yjp253 - gol_im_yjp253) / gol_im_yjp253) * 100; end
if(gol_yjp254[63:32] == 0) begin err_re_per254 = (dut_re_yjp254 - gol_re_yjp254); end else begin err_re_per254 = ((dut_re_yjp254 - gol_re_yjp254) / gol_re_yjp254) * 100; end
if(gol_yjp254[31:0] == 0) begin err_im_per254 = (dut_im_yjp254 - gol_im_yjp254); end else begin err_im_per254 = ((dut_im_yjp254 - gol_im_yjp254) / gol_im_yjp254) * 100; end
if(gol_yjp255[63:32] == 0) begin err_re_per255 = (dut_re_yjp255 - gol_re_yjp255); end else begin err_re_per255 = ((dut_re_yjp255 - gol_re_yjp255) / gol_re_yjp255) * 100; end
if(gol_yjp255[31:0] == 0) begin err_im_per255 = (dut_im_yjp255 - gol_im_yjp255); end else begin err_im_per255 = ((dut_im_yjp255 - gol_im_yjp255) / gol_im_yjp255) * 100; end

if(err_re_per0 < 0) err_re_per0 = -err_re_per0; else err_re_per0 = err_re_per0;
if(err_im_per0 < 0) err_im_per0 = -err_im_per0; else err_im_per0 = err_im_per0;
if(err_re_per1 < 0) err_re_per1 = -err_re_per1; else err_re_per1 = err_re_per1;
if(err_im_per1 < 0) err_im_per1 = -err_im_per1; else err_im_per1 = err_im_per1;
if(err_re_per2 < 0) err_re_per2 = -err_re_per2; else err_re_per2 = err_re_per2;
if(err_im_per2 < 0) err_im_per2 = -err_im_per2; else err_im_per2 = err_im_per2;
if(err_re_per3 < 0) err_re_per3 = -err_re_per3; else err_re_per3 = err_re_per3;
if(err_im_per3 < 0) err_im_per3 = -err_im_per3; else err_im_per3 = err_im_per3;
if(err_re_per4 < 0) err_re_per4 = -err_re_per4; else err_re_per4 = err_re_per4;
if(err_im_per4 < 0) err_im_per4 = -err_im_per4; else err_im_per4 = err_im_per4;
if(err_re_per5 < 0) err_re_per5 = -err_re_per5; else err_re_per5 = err_re_per5;
if(err_im_per5 < 0) err_im_per5 = -err_im_per5; else err_im_per5 = err_im_per5;
if(err_re_per6 < 0) err_re_per6 = -err_re_per6; else err_re_per6 = err_re_per6;
if(err_im_per6 < 0) err_im_per6 = -err_im_per6; else err_im_per6 = err_im_per6;
if(err_re_per7 < 0) err_re_per7 = -err_re_per7; else err_re_per7 = err_re_per7;
if(err_im_per7 < 0) err_im_per7 = -err_im_per7; else err_im_per7 = err_im_per7;
if(err_re_per8 < 0) err_re_per8 = -err_re_per8; else err_re_per8 = err_re_per8;
if(err_im_per8 < 0) err_im_per8 = -err_im_per8; else err_im_per8 = err_im_per8;
if(err_re_per9 < 0) err_re_per9 = -err_re_per9; else err_re_per9 = err_re_per9;
if(err_im_per9 < 0) err_im_per9 = -err_im_per9; else err_im_per9 = err_im_per9;
if(err_re_per10 < 0) err_re_per10 = -err_re_per10; else err_re_per10 = err_re_per10;
if(err_im_per10 < 0) err_im_per10 = -err_im_per10; else err_im_per10 = err_im_per10;
if(err_re_per11 < 0) err_re_per11 = -err_re_per11; else err_re_per11 = err_re_per11;
if(err_im_per11 < 0) err_im_per11 = -err_im_per11; else err_im_per11 = err_im_per11;
if(err_re_per12 < 0) err_re_per12 = -err_re_per12; else err_re_per12 = err_re_per12;
if(err_im_per12 < 0) err_im_per12 = -err_im_per12; else err_im_per12 = err_im_per12;
if(err_re_per13 < 0) err_re_per13 = -err_re_per13; else err_re_per13 = err_re_per13;
if(err_im_per13 < 0) err_im_per13 = -err_im_per13; else err_im_per13 = err_im_per13;
if(err_re_per14 < 0) err_re_per14 = -err_re_per14; else err_re_per14 = err_re_per14;
if(err_im_per14 < 0) err_im_per14 = -err_im_per14; else err_im_per14 = err_im_per14;
if(err_re_per15 < 0) err_re_per15 = -err_re_per15; else err_re_per15 = err_re_per15;
if(err_im_per15 < 0) err_im_per15 = -err_im_per15; else err_im_per15 = err_im_per15;
if(err_re_per16 < 0) err_re_per16 = -err_re_per16; else err_re_per16 = err_re_per16;
if(err_im_per16 < 0) err_im_per16 = -err_im_per16; else err_im_per16 = err_im_per16;
if(err_re_per17 < 0) err_re_per17 = -err_re_per17; else err_re_per17 = err_re_per17;
if(err_im_per17 < 0) err_im_per17 = -err_im_per17; else err_im_per17 = err_im_per17;
if(err_re_per18 < 0) err_re_per18 = -err_re_per18; else err_re_per18 = err_re_per18;
if(err_im_per18 < 0) err_im_per18 = -err_im_per18; else err_im_per18 = err_im_per18;
if(err_re_per19 < 0) err_re_per19 = -err_re_per19; else err_re_per19 = err_re_per19;
if(err_im_per19 < 0) err_im_per19 = -err_im_per19; else err_im_per19 = err_im_per19;
if(err_re_per20 < 0) err_re_per20 = -err_re_per20; else err_re_per20 = err_re_per20;
if(err_im_per20 < 0) err_im_per20 = -err_im_per20; else err_im_per20 = err_im_per20;
if(err_re_per21 < 0) err_re_per21 = -err_re_per21; else err_re_per21 = err_re_per21;
if(err_im_per21 < 0) err_im_per21 = -err_im_per21; else err_im_per21 = err_im_per21;
if(err_re_per22 < 0) err_re_per22 = -err_re_per22; else err_re_per22 = err_re_per22;
if(err_im_per22 < 0) err_im_per22 = -err_im_per22; else err_im_per22 = err_im_per22;
if(err_re_per23 < 0) err_re_per23 = -err_re_per23; else err_re_per23 = err_re_per23;
if(err_im_per23 < 0) err_im_per23 = -err_im_per23; else err_im_per23 = err_im_per23;
if(err_re_per24 < 0) err_re_per24 = -err_re_per24; else err_re_per24 = err_re_per24;
if(err_im_per24 < 0) err_im_per24 = -err_im_per24; else err_im_per24 = err_im_per24;
if(err_re_per25 < 0) err_re_per25 = -err_re_per25; else err_re_per25 = err_re_per25;
if(err_im_per25 < 0) err_im_per25 = -err_im_per25; else err_im_per25 = err_im_per25;
if(err_re_per26 < 0) err_re_per26 = -err_re_per26; else err_re_per26 = err_re_per26;
if(err_im_per26 < 0) err_im_per26 = -err_im_per26; else err_im_per26 = err_im_per26;
if(err_re_per27 < 0) err_re_per27 = -err_re_per27; else err_re_per27 = err_re_per27;
if(err_im_per27 < 0) err_im_per27 = -err_im_per27; else err_im_per27 = err_im_per27;
if(err_re_per28 < 0) err_re_per28 = -err_re_per28; else err_re_per28 = err_re_per28;
if(err_im_per28 < 0) err_im_per28 = -err_im_per28; else err_im_per28 = err_im_per28;
if(err_re_per29 < 0) err_re_per29 = -err_re_per29; else err_re_per29 = err_re_per29;
if(err_im_per29 < 0) err_im_per29 = -err_im_per29; else err_im_per29 = err_im_per29;
if(err_re_per30 < 0) err_re_per30 = -err_re_per30; else err_re_per30 = err_re_per30;
if(err_im_per30 < 0) err_im_per30 = -err_im_per30; else err_im_per30 = err_im_per30;
if(err_re_per31 < 0) err_re_per31 = -err_re_per31; else err_re_per31 = err_re_per31;
if(err_im_per31 < 0) err_im_per31 = -err_im_per31; else err_im_per31 = err_im_per31;
if(err_re_per32 < 0) err_re_per32 = -err_re_per32; else err_re_per32 = err_re_per32;
if(err_im_per32 < 0) err_im_per32 = -err_im_per32; else err_im_per32 = err_im_per32;
if(err_re_per33 < 0) err_re_per33 = -err_re_per33; else err_re_per33 = err_re_per33;
if(err_im_per33 < 0) err_im_per33 = -err_im_per33; else err_im_per33 = err_im_per33;
if(err_re_per34 < 0) err_re_per34 = -err_re_per34; else err_re_per34 = err_re_per34;
if(err_im_per34 < 0) err_im_per34 = -err_im_per34; else err_im_per34 = err_im_per34;
if(err_re_per35 < 0) err_re_per35 = -err_re_per35; else err_re_per35 = err_re_per35;
if(err_im_per35 < 0) err_im_per35 = -err_im_per35; else err_im_per35 = err_im_per35;
if(err_re_per36 < 0) err_re_per36 = -err_re_per36; else err_re_per36 = err_re_per36;
if(err_im_per36 < 0) err_im_per36 = -err_im_per36; else err_im_per36 = err_im_per36;
if(err_re_per37 < 0) err_re_per37 = -err_re_per37; else err_re_per37 = err_re_per37;
if(err_im_per37 < 0) err_im_per37 = -err_im_per37; else err_im_per37 = err_im_per37;
if(err_re_per38 < 0) err_re_per38 = -err_re_per38; else err_re_per38 = err_re_per38;
if(err_im_per38 < 0) err_im_per38 = -err_im_per38; else err_im_per38 = err_im_per38;
if(err_re_per39 < 0) err_re_per39 = -err_re_per39; else err_re_per39 = err_re_per39;
if(err_im_per39 < 0) err_im_per39 = -err_im_per39; else err_im_per39 = err_im_per39;
if(err_re_per40 < 0) err_re_per40 = -err_re_per40; else err_re_per40 = err_re_per40;
if(err_im_per40 < 0) err_im_per40 = -err_im_per40; else err_im_per40 = err_im_per40;
if(err_re_per41 < 0) err_re_per41 = -err_re_per41; else err_re_per41 = err_re_per41;
if(err_im_per41 < 0) err_im_per41 = -err_im_per41; else err_im_per41 = err_im_per41;
if(err_re_per42 < 0) err_re_per42 = -err_re_per42; else err_re_per42 = err_re_per42;
if(err_im_per42 < 0) err_im_per42 = -err_im_per42; else err_im_per42 = err_im_per42;
if(err_re_per43 < 0) err_re_per43 = -err_re_per43; else err_re_per43 = err_re_per43;
if(err_im_per43 < 0) err_im_per43 = -err_im_per43; else err_im_per43 = err_im_per43;
if(err_re_per44 < 0) err_re_per44 = -err_re_per44; else err_re_per44 = err_re_per44;
if(err_im_per44 < 0) err_im_per44 = -err_im_per44; else err_im_per44 = err_im_per44;
if(err_re_per45 < 0) err_re_per45 = -err_re_per45; else err_re_per45 = err_re_per45;
if(err_im_per45 < 0) err_im_per45 = -err_im_per45; else err_im_per45 = err_im_per45;
if(err_re_per46 < 0) err_re_per46 = -err_re_per46; else err_re_per46 = err_re_per46;
if(err_im_per46 < 0) err_im_per46 = -err_im_per46; else err_im_per46 = err_im_per46;
if(err_re_per47 < 0) err_re_per47 = -err_re_per47; else err_re_per47 = err_re_per47;
if(err_im_per47 < 0) err_im_per47 = -err_im_per47; else err_im_per47 = err_im_per47;
if(err_re_per48 < 0) err_re_per48 = -err_re_per48; else err_re_per48 = err_re_per48;
if(err_im_per48 < 0) err_im_per48 = -err_im_per48; else err_im_per48 = err_im_per48;
if(err_re_per49 < 0) err_re_per49 = -err_re_per49; else err_re_per49 = err_re_per49;
if(err_im_per49 < 0) err_im_per49 = -err_im_per49; else err_im_per49 = err_im_per49;
if(err_re_per50 < 0) err_re_per50 = -err_re_per50; else err_re_per50 = err_re_per50;
if(err_im_per50 < 0) err_im_per50 = -err_im_per50; else err_im_per50 = err_im_per50;
if(err_re_per51 < 0) err_re_per51 = -err_re_per51; else err_re_per51 = err_re_per51;
if(err_im_per51 < 0) err_im_per51 = -err_im_per51; else err_im_per51 = err_im_per51;
if(err_re_per52 < 0) err_re_per52 = -err_re_per52; else err_re_per52 = err_re_per52;
if(err_im_per52 < 0) err_im_per52 = -err_im_per52; else err_im_per52 = err_im_per52;
if(err_re_per53 < 0) err_re_per53 = -err_re_per53; else err_re_per53 = err_re_per53;
if(err_im_per53 < 0) err_im_per53 = -err_im_per53; else err_im_per53 = err_im_per53;
if(err_re_per54 < 0) err_re_per54 = -err_re_per54; else err_re_per54 = err_re_per54;
if(err_im_per54 < 0) err_im_per54 = -err_im_per54; else err_im_per54 = err_im_per54;
if(err_re_per55 < 0) err_re_per55 = -err_re_per55; else err_re_per55 = err_re_per55;
if(err_im_per55 < 0) err_im_per55 = -err_im_per55; else err_im_per55 = err_im_per55;
if(err_re_per56 < 0) err_re_per56 = -err_re_per56; else err_re_per56 = err_re_per56;
if(err_im_per56 < 0) err_im_per56 = -err_im_per56; else err_im_per56 = err_im_per56;
if(err_re_per57 < 0) err_re_per57 = -err_re_per57; else err_re_per57 = err_re_per57;
if(err_im_per57 < 0) err_im_per57 = -err_im_per57; else err_im_per57 = err_im_per57;
if(err_re_per58 < 0) err_re_per58 = -err_re_per58; else err_re_per58 = err_re_per58;
if(err_im_per58 < 0) err_im_per58 = -err_im_per58; else err_im_per58 = err_im_per58;
if(err_re_per59 < 0) err_re_per59 = -err_re_per59; else err_re_per59 = err_re_per59;
if(err_im_per59 < 0) err_im_per59 = -err_im_per59; else err_im_per59 = err_im_per59;
if(err_re_per60 < 0) err_re_per60 = -err_re_per60; else err_re_per60 = err_re_per60;
if(err_im_per60 < 0) err_im_per60 = -err_im_per60; else err_im_per60 = err_im_per60;
if(err_re_per61 < 0) err_re_per61 = -err_re_per61; else err_re_per61 = err_re_per61;
if(err_im_per61 < 0) err_im_per61 = -err_im_per61; else err_im_per61 = err_im_per61;
if(err_re_per62 < 0) err_re_per62 = -err_re_per62; else err_re_per62 = err_re_per62;
if(err_im_per62 < 0) err_im_per62 = -err_im_per62; else err_im_per62 = err_im_per62;
if(err_re_per63 < 0) err_re_per63 = -err_re_per63; else err_re_per63 = err_re_per63;
if(err_im_per63 < 0) err_im_per63 = -err_im_per63; else err_im_per63 = err_im_per63;
if(err_re_per64 < 0) err_re_per64 = -err_re_per64; else err_re_per64 = err_re_per64;
if(err_im_per64 < 0) err_im_per64 = -err_im_per64; else err_im_per64 = err_im_per64;
if(err_re_per65 < 0) err_re_per65 = -err_re_per65; else err_re_per65 = err_re_per65;
if(err_im_per65 < 0) err_im_per65 = -err_im_per65; else err_im_per65 = err_im_per65;
if(err_re_per66 < 0) err_re_per66 = -err_re_per66; else err_re_per66 = err_re_per66;
if(err_im_per66 < 0) err_im_per66 = -err_im_per66; else err_im_per66 = err_im_per66;
if(err_re_per67 < 0) err_re_per67 = -err_re_per67; else err_re_per67 = err_re_per67;
if(err_im_per67 < 0) err_im_per67 = -err_im_per67; else err_im_per67 = err_im_per67;
if(err_re_per68 < 0) err_re_per68 = -err_re_per68; else err_re_per68 = err_re_per68;
if(err_im_per68 < 0) err_im_per68 = -err_im_per68; else err_im_per68 = err_im_per68;
if(err_re_per69 < 0) err_re_per69 = -err_re_per69; else err_re_per69 = err_re_per69;
if(err_im_per69 < 0) err_im_per69 = -err_im_per69; else err_im_per69 = err_im_per69;
if(err_re_per70 < 0) err_re_per70 = -err_re_per70; else err_re_per70 = err_re_per70;
if(err_im_per70 < 0) err_im_per70 = -err_im_per70; else err_im_per70 = err_im_per70;
if(err_re_per71 < 0) err_re_per71 = -err_re_per71; else err_re_per71 = err_re_per71;
if(err_im_per71 < 0) err_im_per71 = -err_im_per71; else err_im_per71 = err_im_per71;
if(err_re_per72 < 0) err_re_per72 = -err_re_per72; else err_re_per72 = err_re_per72;
if(err_im_per72 < 0) err_im_per72 = -err_im_per72; else err_im_per72 = err_im_per72;
if(err_re_per73 < 0) err_re_per73 = -err_re_per73; else err_re_per73 = err_re_per73;
if(err_im_per73 < 0) err_im_per73 = -err_im_per73; else err_im_per73 = err_im_per73;
if(err_re_per74 < 0) err_re_per74 = -err_re_per74; else err_re_per74 = err_re_per74;
if(err_im_per74 < 0) err_im_per74 = -err_im_per74; else err_im_per74 = err_im_per74;
if(err_re_per75 < 0) err_re_per75 = -err_re_per75; else err_re_per75 = err_re_per75;
if(err_im_per75 < 0) err_im_per75 = -err_im_per75; else err_im_per75 = err_im_per75;
if(err_re_per76 < 0) err_re_per76 = -err_re_per76; else err_re_per76 = err_re_per76;
if(err_im_per76 < 0) err_im_per76 = -err_im_per76; else err_im_per76 = err_im_per76;
if(err_re_per77 < 0) err_re_per77 = -err_re_per77; else err_re_per77 = err_re_per77;
if(err_im_per77 < 0) err_im_per77 = -err_im_per77; else err_im_per77 = err_im_per77;
if(err_re_per78 < 0) err_re_per78 = -err_re_per78; else err_re_per78 = err_re_per78;
if(err_im_per78 < 0) err_im_per78 = -err_im_per78; else err_im_per78 = err_im_per78;
if(err_re_per79 < 0) err_re_per79 = -err_re_per79; else err_re_per79 = err_re_per79;
if(err_im_per79 < 0) err_im_per79 = -err_im_per79; else err_im_per79 = err_im_per79;
if(err_re_per80 < 0) err_re_per80 = -err_re_per80; else err_re_per80 = err_re_per80;
if(err_im_per80 < 0) err_im_per80 = -err_im_per80; else err_im_per80 = err_im_per80;
if(err_re_per81 < 0) err_re_per81 = -err_re_per81; else err_re_per81 = err_re_per81;
if(err_im_per81 < 0) err_im_per81 = -err_im_per81; else err_im_per81 = err_im_per81;
if(err_re_per82 < 0) err_re_per82 = -err_re_per82; else err_re_per82 = err_re_per82;
if(err_im_per82 < 0) err_im_per82 = -err_im_per82; else err_im_per82 = err_im_per82;
if(err_re_per83 < 0) err_re_per83 = -err_re_per83; else err_re_per83 = err_re_per83;
if(err_im_per83 < 0) err_im_per83 = -err_im_per83; else err_im_per83 = err_im_per83;
if(err_re_per84 < 0) err_re_per84 = -err_re_per84; else err_re_per84 = err_re_per84;
if(err_im_per84 < 0) err_im_per84 = -err_im_per84; else err_im_per84 = err_im_per84;
if(err_re_per85 < 0) err_re_per85 = -err_re_per85; else err_re_per85 = err_re_per85;
if(err_im_per85 < 0) err_im_per85 = -err_im_per85; else err_im_per85 = err_im_per85;
if(err_re_per86 < 0) err_re_per86 = -err_re_per86; else err_re_per86 = err_re_per86;
if(err_im_per86 < 0) err_im_per86 = -err_im_per86; else err_im_per86 = err_im_per86;
if(err_re_per87 < 0) err_re_per87 = -err_re_per87; else err_re_per87 = err_re_per87;
if(err_im_per87 < 0) err_im_per87 = -err_im_per87; else err_im_per87 = err_im_per87;
if(err_re_per88 < 0) err_re_per88 = -err_re_per88; else err_re_per88 = err_re_per88;
if(err_im_per88 < 0) err_im_per88 = -err_im_per88; else err_im_per88 = err_im_per88;
if(err_re_per89 < 0) err_re_per89 = -err_re_per89; else err_re_per89 = err_re_per89;
if(err_im_per89 < 0) err_im_per89 = -err_im_per89; else err_im_per89 = err_im_per89;
if(err_re_per90 < 0) err_re_per90 = -err_re_per90; else err_re_per90 = err_re_per90;
if(err_im_per90 < 0) err_im_per90 = -err_im_per90; else err_im_per90 = err_im_per90;
if(err_re_per91 < 0) err_re_per91 = -err_re_per91; else err_re_per91 = err_re_per91;
if(err_im_per91 < 0) err_im_per91 = -err_im_per91; else err_im_per91 = err_im_per91;
if(err_re_per92 < 0) err_re_per92 = -err_re_per92; else err_re_per92 = err_re_per92;
if(err_im_per92 < 0) err_im_per92 = -err_im_per92; else err_im_per92 = err_im_per92;
if(err_re_per93 < 0) err_re_per93 = -err_re_per93; else err_re_per93 = err_re_per93;
if(err_im_per93 < 0) err_im_per93 = -err_im_per93; else err_im_per93 = err_im_per93;
if(err_re_per94 < 0) err_re_per94 = -err_re_per94; else err_re_per94 = err_re_per94;
if(err_im_per94 < 0) err_im_per94 = -err_im_per94; else err_im_per94 = err_im_per94;
if(err_re_per95 < 0) err_re_per95 = -err_re_per95; else err_re_per95 = err_re_per95;
if(err_im_per95 < 0) err_im_per95 = -err_im_per95; else err_im_per95 = err_im_per95;
if(err_re_per96 < 0) err_re_per96 = -err_re_per96; else err_re_per96 = err_re_per96;
if(err_im_per96 < 0) err_im_per96 = -err_im_per96; else err_im_per96 = err_im_per96;
if(err_re_per97 < 0) err_re_per97 = -err_re_per97; else err_re_per97 = err_re_per97;
if(err_im_per97 < 0) err_im_per97 = -err_im_per97; else err_im_per97 = err_im_per97;
if(err_re_per98 < 0) err_re_per98 = -err_re_per98; else err_re_per98 = err_re_per98;
if(err_im_per98 < 0) err_im_per98 = -err_im_per98; else err_im_per98 = err_im_per98;
if(err_re_per99 < 0) err_re_per99 = -err_re_per99; else err_re_per99 = err_re_per99;
if(err_im_per99 < 0) err_im_per99 = -err_im_per99; else err_im_per99 = err_im_per99;
if(err_re_per100 < 0) err_re_per100 = -err_re_per100; else err_re_per100 = err_re_per100;
if(err_im_per100 < 0) err_im_per100 = -err_im_per100; else err_im_per100 = err_im_per100;
if(err_re_per101 < 0) err_re_per101 = -err_re_per101; else err_re_per101 = err_re_per101;
if(err_im_per101 < 0) err_im_per101 = -err_im_per101; else err_im_per101 = err_im_per101;
if(err_re_per102 < 0) err_re_per102 = -err_re_per102; else err_re_per102 = err_re_per102;
if(err_im_per102 < 0) err_im_per102 = -err_im_per102; else err_im_per102 = err_im_per102;
if(err_re_per103 < 0) err_re_per103 = -err_re_per103; else err_re_per103 = err_re_per103;
if(err_im_per103 < 0) err_im_per103 = -err_im_per103; else err_im_per103 = err_im_per103;
if(err_re_per104 < 0) err_re_per104 = -err_re_per104; else err_re_per104 = err_re_per104;
if(err_im_per104 < 0) err_im_per104 = -err_im_per104; else err_im_per104 = err_im_per104;
if(err_re_per105 < 0) err_re_per105 = -err_re_per105; else err_re_per105 = err_re_per105;
if(err_im_per105 < 0) err_im_per105 = -err_im_per105; else err_im_per105 = err_im_per105;
if(err_re_per106 < 0) err_re_per106 = -err_re_per106; else err_re_per106 = err_re_per106;
if(err_im_per106 < 0) err_im_per106 = -err_im_per106; else err_im_per106 = err_im_per106;
if(err_re_per107 < 0) err_re_per107 = -err_re_per107; else err_re_per107 = err_re_per107;
if(err_im_per107 < 0) err_im_per107 = -err_im_per107; else err_im_per107 = err_im_per107;
if(err_re_per108 < 0) err_re_per108 = -err_re_per108; else err_re_per108 = err_re_per108;
if(err_im_per108 < 0) err_im_per108 = -err_im_per108; else err_im_per108 = err_im_per108;
if(err_re_per109 < 0) err_re_per109 = -err_re_per109; else err_re_per109 = err_re_per109;
if(err_im_per109 < 0) err_im_per109 = -err_im_per109; else err_im_per109 = err_im_per109;
if(err_re_per110 < 0) err_re_per110 = -err_re_per110; else err_re_per110 = err_re_per110;
if(err_im_per110 < 0) err_im_per110 = -err_im_per110; else err_im_per110 = err_im_per110;
if(err_re_per111 < 0) err_re_per111 = -err_re_per111; else err_re_per111 = err_re_per111;
if(err_im_per111 < 0) err_im_per111 = -err_im_per111; else err_im_per111 = err_im_per111;
if(err_re_per112 < 0) err_re_per112 = -err_re_per112; else err_re_per112 = err_re_per112;
if(err_im_per112 < 0) err_im_per112 = -err_im_per112; else err_im_per112 = err_im_per112;
if(err_re_per113 < 0) err_re_per113 = -err_re_per113; else err_re_per113 = err_re_per113;
if(err_im_per113 < 0) err_im_per113 = -err_im_per113; else err_im_per113 = err_im_per113;
if(err_re_per114 < 0) err_re_per114 = -err_re_per114; else err_re_per114 = err_re_per114;
if(err_im_per114 < 0) err_im_per114 = -err_im_per114; else err_im_per114 = err_im_per114;
if(err_re_per115 < 0) err_re_per115 = -err_re_per115; else err_re_per115 = err_re_per115;
if(err_im_per115 < 0) err_im_per115 = -err_im_per115; else err_im_per115 = err_im_per115;
if(err_re_per116 < 0) err_re_per116 = -err_re_per116; else err_re_per116 = err_re_per116;
if(err_im_per116 < 0) err_im_per116 = -err_im_per116; else err_im_per116 = err_im_per116;
if(err_re_per117 < 0) err_re_per117 = -err_re_per117; else err_re_per117 = err_re_per117;
if(err_im_per117 < 0) err_im_per117 = -err_im_per117; else err_im_per117 = err_im_per117;
if(err_re_per118 < 0) err_re_per118 = -err_re_per118; else err_re_per118 = err_re_per118;
if(err_im_per118 < 0) err_im_per118 = -err_im_per118; else err_im_per118 = err_im_per118;
if(err_re_per119 < 0) err_re_per119 = -err_re_per119; else err_re_per119 = err_re_per119;
if(err_im_per119 < 0) err_im_per119 = -err_im_per119; else err_im_per119 = err_im_per119;
if(err_re_per120 < 0) err_re_per120 = -err_re_per120; else err_re_per120 = err_re_per120;
if(err_im_per120 < 0) err_im_per120 = -err_im_per120; else err_im_per120 = err_im_per120;
if(err_re_per121 < 0) err_re_per121 = -err_re_per121; else err_re_per121 = err_re_per121;
if(err_im_per121 < 0) err_im_per121 = -err_im_per121; else err_im_per121 = err_im_per121;
if(err_re_per122 < 0) err_re_per122 = -err_re_per122; else err_re_per122 = err_re_per122;
if(err_im_per122 < 0) err_im_per122 = -err_im_per122; else err_im_per122 = err_im_per122;
if(err_re_per123 < 0) err_re_per123 = -err_re_per123; else err_re_per123 = err_re_per123;
if(err_im_per123 < 0) err_im_per123 = -err_im_per123; else err_im_per123 = err_im_per123;
if(err_re_per124 < 0) err_re_per124 = -err_re_per124; else err_re_per124 = err_re_per124;
if(err_im_per124 < 0) err_im_per124 = -err_im_per124; else err_im_per124 = err_im_per124;
if(err_re_per125 < 0) err_re_per125 = -err_re_per125; else err_re_per125 = err_re_per125;
if(err_im_per125 < 0) err_im_per125 = -err_im_per125; else err_im_per125 = err_im_per125;
if(err_re_per126 < 0) err_re_per126 = -err_re_per126; else err_re_per126 = err_re_per126;
if(err_im_per126 < 0) err_im_per126 = -err_im_per126; else err_im_per126 = err_im_per126;
if(err_re_per127 < 0) err_re_per127 = -err_re_per127; else err_re_per127 = err_re_per127;
if(err_im_per127 < 0) err_im_per127 = -err_im_per127; else err_im_per127 = err_im_per127;
if(err_re_per128 < 0) err_re_per128 = -err_re_per128; else err_re_per128 = err_re_per128;
if(err_im_per128 < 0) err_im_per128 = -err_im_per128; else err_im_per128 = err_im_per128;
if(err_re_per129 < 0) err_re_per129 = -err_re_per129; else err_re_per129 = err_re_per129;
if(err_im_per129 < 0) err_im_per129 = -err_im_per129; else err_im_per129 = err_im_per129;
if(err_re_per130 < 0) err_re_per130 = -err_re_per130; else err_re_per130 = err_re_per130;
if(err_im_per130 < 0) err_im_per130 = -err_im_per130; else err_im_per130 = err_im_per130;
if(err_re_per131 < 0) err_re_per131 = -err_re_per131; else err_re_per131 = err_re_per131;
if(err_im_per131 < 0) err_im_per131 = -err_im_per131; else err_im_per131 = err_im_per131;
if(err_re_per132 < 0) err_re_per132 = -err_re_per132; else err_re_per132 = err_re_per132;
if(err_im_per132 < 0) err_im_per132 = -err_im_per132; else err_im_per132 = err_im_per132;
if(err_re_per133 < 0) err_re_per133 = -err_re_per133; else err_re_per133 = err_re_per133;
if(err_im_per133 < 0) err_im_per133 = -err_im_per133; else err_im_per133 = err_im_per133;
if(err_re_per134 < 0) err_re_per134 = -err_re_per134; else err_re_per134 = err_re_per134;
if(err_im_per134 < 0) err_im_per134 = -err_im_per134; else err_im_per134 = err_im_per134;
if(err_re_per135 < 0) err_re_per135 = -err_re_per135; else err_re_per135 = err_re_per135;
if(err_im_per135 < 0) err_im_per135 = -err_im_per135; else err_im_per135 = err_im_per135;
if(err_re_per136 < 0) err_re_per136 = -err_re_per136; else err_re_per136 = err_re_per136;
if(err_im_per136 < 0) err_im_per136 = -err_im_per136; else err_im_per136 = err_im_per136;
if(err_re_per137 < 0) err_re_per137 = -err_re_per137; else err_re_per137 = err_re_per137;
if(err_im_per137 < 0) err_im_per137 = -err_im_per137; else err_im_per137 = err_im_per137;
if(err_re_per138 < 0) err_re_per138 = -err_re_per138; else err_re_per138 = err_re_per138;
if(err_im_per138 < 0) err_im_per138 = -err_im_per138; else err_im_per138 = err_im_per138;
if(err_re_per139 < 0) err_re_per139 = -err_re_per139; else err_re_per139 = err_re_per139;
if(err_im_per139 < 0) err_im_per139 = -err_im_per139; else err_im_per139 = err_im_per139;
if(err_re_per140 < 0) err_re_per140 = -err_re_per140; else err_re_per140 = err_re_per140;
if(err_im_per140 < 0) err_im_per140 = -err_im_per140; else err_im_per140 = err_im_per140;
if(err_re_per141 < 0) err_re_per141 = -err_re_per141; else err_re_per141 = err_re_per141;
if(err_im_per141 < 0) err_im_per141 = -err_im_per141; else err_im_per141 = err_im_per141;
if(err_re_per142 < 0) err_re_per142 = -err_re_per142; else err_re_per142 = err_re_per142;
if(err_im_per142 < 0) err_im_per142 = -err_im_per142; else err_im_per142 = err_im_per142;
if(err_re_per143 < 0) err_re_per143 = -err_re_per143; else err_re_per143 = err_re_per143;
if(err_im_per143 < 0) err_im_per143 = -err_im_per143; else err_im_per143 = err_im_per143;
if(err_re_per144 < 0) err_re_per144 = -err_re_per144; else err_re_per144 = err_re_per144;
if(err_im_per144 < 0) err_im_per144 = -err_im_per144; else err_im_per144 = err_im_per144;
if(err_re_per145 < 0) err_re_per145 = -err_re_per145; else err_re_per145 = err_re_per145;
if(err_im_per145 < 0) err_im_per145 = -err_im_per145; else err_im_per145 = err_im_per145;
if(err_re_per146 < 0) err_re_per146 = -err_re_per146; else err_re_per146 = err_re_per146;
if(err_im_per146 < 0) err_im_per146 = -err_im_per146; else err_im_per146 = err_im_per146;
if(err_re_per147 < 0) err_re_per147 = -err_re_per147; else err_re_per147 = err_re_per147;
if(err_im_per147 < 0) err_im_per147 = -err_im_per147; else err_im_per147 = err_im_per147;
if(err_re_per148 < 0) err_re_per148 = -err_re_per148; else err_re_per148 = err_re_per148;
if(err_im_per148 < 0) err_im_per148 = -err_im_per148; else err_im_per148 = err_im_per148;
if(err_re_per149 < 0) err_re_per149 = -err_re_per149; else err_re_per149 = err_re_per149;
if(err_im_per149 < 0) err_im_per149 = -err_im_per149; else err_im_per149 = err_im_per149;
if(err_re_per150 < 0) err_re_per150 = -err_re_per150; else err_re_per150 = err_re_per150;
if(err_im_per150 < 0) err_im_per150 = -err_im_per150; else err_im_per150 = err_im_per150;
if(err_re_per151 < 0) err_re_per151 = -err_re_per151; else err_re_per151 = err_re_per151;
if(err_im_per151 < 0) err_im_per151 = -err_im_per151; else err_im_per151 = err_im_per151;
if(err_re_per152 < 0) err_re_per152 = -err_re_per152; else err_re_per152 = err_re_per152;
if(err_im_per152 < 0) err_im_per152 = -err_im_per152; else err_im_per152 = err_im_per152;
if(err_re_per153 < 0) err_re_per153 = -err_re_per153; else err_re_per153 = err_re_per153;
if(err_im_per153 < 0) err_im_per153 = -err_im_per153; else err_im_per153 = err_im_per153;
if(err_re_per154 < 0) err_re_per154 = -err_re_per154; else err_re_per154 = err_re_per154;
if(err_im_per154 < 0) err_im_per154 = -err_im_per154; else err_im_per154 = err_im_per154;
if(err_re_per155 < 0) err_re_per155 = -err_re_per155; else err_re_per155 = err_re_per155;
if(err_im_per155 < 0) err_im_per155 = -err_im_per155; else err_im_per155 = err_im_per155;
if(err_re_per156 < 0) err_re_per156 = -err_re_per156; else err_re_per156 = err_re_per156;
if(err_im_per156 < 0) err_im_per156 = -err_im_per156; else err_im_per156 = err_im_per156;
if(err_re_per157 < 0) err_re_per157 = -err_re_per157; else err_re_per157 = err_re_per157;
if(err_im_per157 < 0) err_im_per157 = -err_im_per157; else err_im_per157 = err_im_per157;
if(err_re_per158 < 0) err_re_per158 = -err_re_per158; else err_re_per158 = err_re_per158;
if(err_im_per158 < 0) err_im_per158 = -err_im_per158; else err_im_per158 = err_im_per158;
if(err_re_per159 < 0) err_re_per159 = -err_re_per159; else err_re_per159 = err_re_per159;
if(err_im_per159 < 0) err_im_per159 = -err_im_per159; else err_im_per159 = err_im_per159;
if(err_re_per160 < 0) err_re_per160 = -err_re_per160; else err_re_per160 = err_re_per160;
if(err_im_per160 < 0) err_im_per160 = -err_im_per160; else err_im_per160 = err_im_per160;
if(err_re_per161 < 0) err_re_per161 = -err_re_per161; else err_re_per161 = err_re_per161;
if(err_im_per161 < 0) err_im_per161 = -err_im_per161; else err_im_per161 = err_im_per161;
if(err_re_per162 < 0) err_re_per162 = -err_re_per162; else err_re_per162 = err_re_per162;
if(err_im_per162 < 0) err_im_per162 = -err_im_per162; else err_im_per162 = err_im_per162;
if(err_re_per163 < 0) err_re_per163 = -err_re_per163; else err_re_per163 = err_re_per163;
if(err_im_per163 < 0) err_im_per163 = -err_im_per163; else err_im_per163 = err_im_per163;
if(err_re_per164 < 0) err_re_per164 = -err_re_per164; else err_re_per164 = err_re_per164;
if(err_im_per164 < 0) err_im_per164 = -err_im_per164; else err_im_per164 = err_im_per164;
if(err_re_per165 < 0) err_re_per165 = -err_re_per165; else err_re_per165 = err_re_per165;
if(err_im_per165 < 0) err_im_per165 = -err_im_per165; else err_im_per165 = err_im_per165;
if(err_re_per166 < 0) err_re_per166 = -err_re_per166; else err_re_per166 = err_re_per166;
if(err_im_per166 < 0) err_im_per166 = -err_im_per166; else err_im_per166 = err_im_per166;
if(err_re_per167 < 0) err_re_per167 = -err_re_per167; else err_re_per167 = err_re_per167;
if(err_im_per167 < 0) err_im_per167 = -err_im_per167; else err_im_per167 = err_im_per167;
if(err_re_per168 < 0) err_re_per168 = -err_re_per168; else err_re_per168 = err_re_per168;
if(err_im_per168 < 0) err_im_per168 = -err_im_per168; else err_im_per168 = err_im_per168;
if(err_re_per169 < 0) err_re_per169 = -err_re_per169; else err_re_per169 = err_re_per169;
if(err_im_per169 < 0) err_im_per169 = -err_im_per169; else err_im_per169 = err_im_per169;
if(err_re_per170 < 0) err_re_per170 = -err_re_per170; else err_re_per170 = err_re_per170;
if(err_im_per170 < 0) err_im_per170 = -err_im_per170; else err_im_per170 = err_im_per170;
if(err_re_per171 < 0) err_re_per171 = -err_re_per171; else err_re_per171 = err_re_per171;
if(err_im_per171 < 0) err_im_per171 = -err_im_per171; else err_im_per171 = err_im_per171;
if(err_re_per172 < 0) err_re_per172 = -err_re_per172; else err_re_per172 = err_re_per172;
if(err_im_per172 < 0) err_im_per172 = -err_im_per172; else err_im_per172 = err_im_per172;
if(err_re_per173 < 0) err_re_per173 = -err_re_per173; else err_re_per173 = err_re_per173;
if(err_im_per173 < 0) err_im_per173 = -err_im_per173; else err_im_per173 = err_im_per173;
if(err_re_per174 < 0) err_re_per174 = -err_re_per174; else err_re_per174 = err_re_per174;
if(err_im_per174 < 0) err_im_per174 = -err_im_per174; else err_im_per174 = err_im_per174;
if(err_re_per175 < 0) err_re_per175 = -err_re_per175; else err_re_per175 = err_re_per175;
if(err_im_per175 < 0) err_im_per175 = -err_im_per175; else err_im_per175 = err_im_per175;
if(err_re_per176 < 0) err_re_per176 = -err_re_per176; else err_re_per176 = err_re_per176;
if(err_im_per176 < 0) err_im_per176 = -err_im_per176; else err_im_per176 = err_im_per176;
if(err_re_per177 < 0) err_re_per177 = -err_re_per177; else err_re_per177 = err_re_per177;
if(err_im_per177 < 0) err_im_per177 = -err_im_per177; else err_im_per177 = err_im_per177;
if(err_re_per178 < 0) err_re_per178 = -err_re_per178; else err_re_per178 = err_re_per178;
if(err_im_per178 < 0) err_im_per178 = -err_im_per178; else err_im_per178 = err_im_per178;
if(err_re_per179 < 0) err_re_per179 = -err_re_per179; else err_re_per179 = err_re_per179;
if(err_im_per179 < 0) err_im_per179 = -err_im_per179; else err_im_per179 = err_im_per179;
if(err_re_per180 < 0) err_re_per180 = -err_re_per180; else err_re_per180 = err_re_per180;
if(err_im_per180 < 0) err_im_per180 = -err_im_per180; else err_im_per180 = err_im_per180;
if(err_re_per181 < 0) err_re_per181 = -err_re_per181; else err_re_per181 = err_re_per181;
if(err_im_per181 < 0) err_im_per181 = -err_im_per181; else err_im_per181 = err_im_per181;
if(err_re_per182 < 0) err_re_per182 = -err_re_per182; else err_re_per182 = err_re_per182;
if(err_im_per182 < 0) err_im_per182 = -err_im_per182; else err_im_per182 = err_im_per182;
if(err_re_per183 < 0) err_re_per183 = -err_re_per183; else err_re_per183 = err_re_per183;
if(err_im_per183 < 0) err_im_per183 = -err_im_per183; else err_im_per183 = err_im_per183;
if(err_re_per184 < 0) err_re_per184 = -err_re_per184; else err_re_per184 = err_re_per184;
if(err_im_per184 < 0) err_im_per184 = -err_im_per184; else err_im_per184 = err_im_per184;
if(err_re_per185 < 0) err_re_per185 = -err_re_per185; else err_re_per185 = err_re_per185;
if(err_im_per185 < 0) err_im_per185 = -err_im_per185; else err_im_per185 = err_im_per185;
if(err_re_per186 < 0) err_re_per186 = -err_re_per186; else err_re_per186 = err_re_per186;
if(err_im_per186 < 0) err_im_per186 = -err_im_per186; else err_im_per186 = err_im_per186;
if(err_re_per187 < 0) err_re_per187 = -err_re_per187; else err_re_per187 = err_re_per187;
if(err_im_per187 < 0) err_im_per187 = -err_im_per187; else err_im_per187 = err_im_per187;
if(err_re_per188 < 0) err_re_per188 = -err_re_per188; else err_re_per188 = err_re_per188;
if(err_im_per188 < 0) err_im_per188 = -err_im_per188; else err_im_per188 = err_im_per188;
if(err_re_per189 < 0) err_re_per189 = -err_re_per189; else err_re_per189 = err_re_per189;
if(err_im_per189 < 0) err_im_per189 = -err_im_per189; else err_im_per189 = err_im_per189;
if(err_re_per190 < 0) err_re_per190 = -err_re_per190; else err_re_per190 = err_re_per190;
if(err_im_per190 < 0) err_im_per190 = -err_im_per190; else err_im_per190 = err_im_per190;
if(err_re_per191 < 0) err_re_per191 = -err_re_per191; else err_re_per191 = err_re_per191;
if(err_im_per191 < 0) err_im_per191 = -err_im_per191; else err_im_per191 = err_im_per191;
if(err_re_per192 < 0) err_re_per192 = -err_re_per192; else err_re_per192 = err_re_per192;
if(err_im_per192 < 0) err_im_per192 = -err_im_per192; else err_im_per192 = err_im_per192;
if(err_re_per193 < 0) err_re_per193 = -err_re_per193; else err_re_per193 = err_re_per193;
if(err_im_per193 < 0) err_im_per193 = -err_im_per193; else err_im_per193 = err_im_per193;
if(err_re_per194 < 0) err_re_per194 = -err_re_per194; else err_re_per194 = err_re_per194;
if(err_im_per194 < 0) err_im_per194 = -err_im_per194; else err_im_per194 = err_im_per194;
if(err_re_per195 < 0) err_re_per195 = -err_re_per195; else err_re_per195 = err_re_per195;
if(err_im_per195 < 0) err_im_per195 = -err_im_per195; else err_im_per195 = err_im_per195;
if(err_re_per196 < 0) err_re_per196 = -err_re_per196; else err_re_per196 = err_re_per196;
if(err_im_per196 < 0) err_im_per196 = -err_im_per196; else err_im_per196 = err_im_per196;
if(err_re_per197 < 0) err_re_per197 = -err_re_per197; else err_re_per197 = err_re_per197;
if(err_im_per197 < 0) err_im_per197 = -err_im_per197; else err_im_per197 = err_im_per197;
if(err_re_per198 < 0) err_re_per198 = -err_re_per198; else err_re_per198 = err_re_per198;
if(err_im_per198 < 0) err_im_per198 = -err_im_per198; else err_im_per198 = err_im_per198;
if(err_re_per199 < 0) err_re_per199 = -err_re_per199; else err_re_per199 = err_re_per199;
if(err_im_per199 < 0) err_im_per199 = -err_im_per199; else err_im_per199 = err_im_per199;
if(err_re_per200 < 0) err_re_per200 = -err_re_per200; else err_re_per200 = err_re_per200;
if(err_im_per200 < 0) err_im_per200 = -err_im_per200; else err_im_per200 = err_im_per200;
if(err_re_per201 < 0) err_re_per201 = -err_re_per201; else err_re_per201 = err_re_per201;
if(err_im_per201 < 0) err_im_per201 = -err_im_per201; else err_im_per201 = err_im_per201;
if(err_re_per202 < 0) err_re_per202 = -err_re_per202; else err_re_per202 = err_re_per202;
if(err_im_per202 < 0) err_im_per202 = -err_im_per202; else err_im_per202 = err_im_per202;
if(err_re_per203 < 0) err_re_per203 = -err_re_per203; else err_re_per203 = err_re_per203;
if(err_im_per203 < 0) err_im_per203 = -err_im_per203; else err_im_per203 = err_im_per203;
if(err_re_per204 < 0) err_re_per204 = -err_re_per204; else err_re_per204 = err_re_per204;
if(err_im_per204 < 0) err_im_per204 = -err_im_per204; else err_im_per204 = err_im_per204;
if(err_re_per205 < 0) err_re_per205 = -err_re_per205; else err_re_per205 = err_re_per205;
if(err_im_per205 < 0) err_im_per205 = -err_im_per205; else err_im_per205 = err_im_per205;
if(err_re_per206 < 0) err_re_per206 = -err_re_per206; else err_re_per206 = err_re_per206;
if(err_im_per206 < 0) err_im_per206 = -err_im_per206; else err_im_per206 = err_im_per206;
if(err_re_per207 < 0) err_re_per207 = -err_re_per207; else err_re_per207 = err_re_per207;
if(err_im_per207 < 0) err_im_per207 = -err_im_per207; else err_im_per207 = err_im_per207;
if(err_re_per208 < 0) err_re_per208 = -err_re_per208; else err_re_per208 = err_re_per208;
if(err_im_per208 < 0) err_im_per208 = -err_im_per208; else err_im_per208 = err_im_per208;
if(err_re_per209 < 0) err_re_per209 = -err_re_per209; else err_re_per209 = err_re_per209;
if(err_im_per209 < 0) err_im_per209 = -err_im_per209; else err_im_per209 = err_im_per209;
if(err_re_per210 < 0) err_re_per210 = -err_re_per210; else err_re_per210 = err_re_per210;
if(err_im_per210 < 0) err_im_per210 = -err_im_per210; else err_im_per210 = err_im_per210;
if(err_re_per211 < 0) err_re_per211 = -err_re_per211; else err_re_per211 = err_re_per211;
if(err_im_per211 < 0) err_im_per211 = -err_im_per211; else err_im_per211 = err_im_per211;
if(err_re_per212 < 0) err_re_per212 = -err_re_per212; else err_re_per212 = err_re_per212;
if(err_im_per212 < 0) err_im_per212 = -err_im_per212; else err_im_per212 = err_im_per212;
if(err_re_per213 < 0) err_re_per213 = -err_re_per213; else err_re_per213 = err_re_per213;
if(err_im_per213 < 0) err_im_per213 = -err_im_per213; else err_im_per213 = err_im_per213;
if(err_re_per214 < 0) err_re_per214 = -err_re_per214; else err_re_per214 = err_re_per214;
if(err_im_per214 < 0) err_im_per214 = -err_im_per214; else err_im_per214 = err_im_per214;
if(err_re_per215 < 0) err_re_per215 = -err_re_per215; else err_re_per215 = err_re_per215;
if(err_im_per215 < 0) err_im_per215 = -err_im_per215; else err_im_per215 = err_im_per215;
if(err_re_per216 < 0) err_re_per216 = -err_re_per216; else err_re_per216 = err_re_per216;
if(err_im_per216 < 0) err_im_per216 = -err_im_per216; else err_im_per216 = err_im_per216;
if(err_re_per217 < 0) err_re_per217 = -err_re_per217; else err_re_per217 = err_re_per217;
if(err_im_per217 < 0) err_im_per217 = -err_im_per217; else err_im_per217 = err_im_per217;
if(err_re_per218 < 0) err_re_per218 = -err_re_per218; else err_re_per218 = err_re_per218;
if(err_im_per218 < 0) err_im_per218 = -err_im_per218; else err_im_per218 = err_im_per218;
if(err_re_per219 < 0) err_re_per219 = -err_re_per219; else err_re_per219 = err_re_per219;
if(err_im_per219 < 0) err_im_per219 = -err_im_per219; else err_im_per219 = err_im_per219;
if(err_re_per220 < 0) err_re_per220 = -err_re_per220; else err_re_per220 = err_re_per220;
if(err_im_per220 < 0) err_im_per220 = -err_im_per220; else err_im_per220 = err_im_per220;
if(err_re_per221 < 0) err_re_per221 = -err_re_per221; else err_re_per221 = err_re_per221;
if(err_im_per221 < 0) err_im_per221 = -err_im_per221; else err_im_per221 = err_im_per221;
if(err_re_per222 < 0) err_re_per222 = -err_re_per222; else err_re_per222 = err_re_per222;
if(err_im_per222 < 0) err_im_per222 = -err_im_per222; else err_im_per222 = err_im_per222;
if(err_re_per223 < 0) err_re_per223 = -err_re_per223; else err_re_per223 = err_re_per223;
if(err_im_per223 < 0) err_im_per223 = -err_im_per223; else err_im_per223 = err_im_per223;
if(err_re_per224 < 0) err_re_per224 = -err_re_per224; else err_re_per224 = err_re_per224;
if(err_im_per224 < 0) err_im_per224 = -err_im_per224; else err_im_per224 = err_im_per224;
if(err_re_per225 < 0) err_re_per225 = -err_re_per225; else err_re_per225 = err_re_per225;
if(err_im_per225 < 0) err_im_per225 = -err_im_per225; else err_im_per225 = err_im_per225;
if(err_re_per226 < 0) err_re_per226 = -err_re_per226; else err_re_per226 = err_re_per226;
if(err_im_per226 < 0) err_im_per226 = -err_im_per226; else err_im_per226 = err_im_per226;
if(err_re_per227 < 0) err_re_per227 = -err_re_per227; else err_re_per227 = err_re_per227;
if(err_im_per227 < 0) err_im_per227 = -err_im_per227; else err_im_per227 = err_im_per227;
if(err_re_per228 < 0) err_re_per228 = -err_re_per228; else err_re_per228 = err_re_per228;
if(err_im_per228 < 0) err_im_per228 = -err_im_per228; else err_im_per228 = err_im_per228;
if(err_re_per229 < 0) err_re_per229 = -err_re_per229; else err_re_per229 = err_re_per229;
if(err_im_per229 < 0) err_im_per229 = -err_im_per229; else err_im_per229 = err_im_per229;
if(err_re_per230 < 0) err_re_per230 = -err_re_per230; else err_re_per230 = err_re_per230;
if(err_im_per230 < 0) err_im_per230 = -err_im_per230; else err_im_per230 = err_im_per230;
if(err_re_per231 < 0) err_re_per231 = -err_re_per231; else err_re_per231 = err_re_per231;
if(err_im_per231 < 0) err_im_per231 = -err_im_per231; else err_im_per231 = err_im_per231;
if(err_re_per232 < 0) err_re_per232 = -err_re_per232; else err_re_per232 = err_re_per232;
if(err_im_per232 < 0) err_im_per232 = -err_im_per232; else err_im_per232 = err_im_per232;
if(err_re_per233 < 0) err_re_per233 = -err_re_per233; else err_re_per233 = err_re_per233;
if(err_im_per233 < 0) err_im_per233 = -err_im_per233; else err_im_per233 = err_im_per233;
if(err_re_per234 < 0) err_re_per234 = -err_re_per234; else err_re_per234 = err_re_per234;
if(err_im_per234 < 0) err_im_per234 = -err_im_per234; else err_im_per234 = err_im_per234;
if(err_re_per235 < 0) err_re_per235 = -err_re_per235; else err_re_per235 = err_re_per235;
if(err_im_per235 < 0) err_im_per235 = -err_im_per235; else err_im_per235 = err_im_per235;
if(err_re_per236 < 0) err_re_per236 = -err_re_per236; else err_re_per236 = err_re_per236;
if(err_im_per236 < 0) err_im_per236 = -err_im_per236; else err_im_per236 = err_im_per236;
if(err_re_per237 < 0) err_re_per237 = -err_re_per237; else err_re_per237 = err_re_per237;
if(err_im_per237 < 0) err_im_per237 = -err_im_per237; else err_im_per237 = err_im_per237;
if(err_re_per238 < 0) err_re_per238 = -err_re_per238; else err_re_per238 = err_re_per238;
if(err_im_per238 < 0) err_im_per238 = -err_im_per238; else err_im_per238 = err_im_per238;
if(err_re_per239 < 0) err_re_per239 = -err_re_per239; else err_re_per239 = err_re_per239;
if(err_im_per239 < 0) err_im_per239 = -err_im_per239; else err_im_per239 = err_im_per239;
if(err_re_per240 < 0) err_re_per240 = -err_re_per240; else err_re_per240 = err_re_per240;
if(err_im_per240 < 0) err_im_per240 = -err_im_per240; else err_im_per240 = err_im_per240;
if(err_re_per241 < 0) err_re_per241 = -err_re_per241; else err_re_per241 = err_re_per241;
if(err_im_per241 < 0) err_im_per241 = -err_im_per241; else err_im_per241 = err_im_per241;
if(err_re_per242 < 0) err_re_per242 = -err_re_per242; else err_re_per242 = err_re_per242;
if(err_im_per242 < 0) err_im_per242 = -err_im_per242; else err_im_per242 = err_im_per242;
if(err_re_per243 < 0) err_re_per243 = -err_re_per243; else err_re_per243 = err_re_per243;
if(err_im_per243 < 0) err_im_per243 = -err_im_per243; else err_im_per243 = err_im_per243;
if(err_re_per244 < 0) err_re_per244 = -err_re_per244; else err_re_per244 = err_re_per244;
if(err_im_per244 < 0) err_im_per244 = -err_im_per244; else err_im_per244 = err_im_per244;
if(err_re_per245 < 0) err_re_per245 = -err_re_per245; else err_re_per245 = err_re_per245;
if(err_im_per245 < 0) err_im_per245 = -err_im_per245; else err_im_per245 = err_im_per245;
if(err_re_per246 < 0) err_re_per246 = -err_re_per246; else err_re_per246 = err_re_per246;
if(err_im_per246 < 0) err_im_per246 = -err_im_per246; else err_im_per246 = err_im_per246;
if(err_re_per247 < 0) err_re_per247 = -err_re_per247; else err_re_per247 = err_re_per247;
if(err_im_per247 < 0) err_im_per247 = -err_im_per247; else err_im_per247 = err_im_per247;
if(err_re_per248 < 0) err_re_per248 = -err_re_per248; else err_re_per248 = err_re_per248;
if(err_im_per248 < 0) err_im_per248 = -err_im_per248; else err_im_per248 = err_im_per248;
if(err_re_per249 < 0) err_re_per249 = -err_re_per249; else err_re_per249 = err_re_per249;
if(err_im_per249 < 0) err_im_per249 = -err_im_per249; else err_im_per249 = err_im_per249;
if(err_re_per250 < 0) err_re_per250 = -err_re_per250; else err_re_per250 = err_re_per250;
if(err_im_per250 < 0) err_im_per250 = -err_im_per250; else err_im_per250 = err_im_per250;
if(err_re_per251 < 0) err_re_per251 = -err_re_per251; else err_re_per251 = err_re_per251;
if(err_im_per251 < 0) err_im_per251 = -err_im_per251; else err_im_per251 = err_im_per251;
if(err_re_per252 < 0) err_re_per252 = -err_re_per252; else err_re_per252 = err_re_per252;
if(err_im_per252 < 0) err_im_per252 = -err_im_per252; else err_im_per252 = err_im_per252;
if(err_re_per253 < 0) err_re_per253 = -err_re_per253; else err_re_per253 = err_re_per253;
if(err_im_per253 < 0) err_im_per253 = -err_im_per253; else err_im_per253 = err_im_per253;
if(err_re_per254 < 0) err_re_per254 = -err_re_per254; else err_re_per254 = err_re_per254;
if(err_im_per254 < 0) err_im_per254 = -err_im_per254; else err_im_per254 = err_im_per254;
if(err_re_per255 < 0) err_re_per255 = -err_re_per255; else err_re_per255 = err_re_per255;
if(err_im_per255 < 0) err_im_per255 = -err_im_per255; else err_im_per255 = err_im_per255;


`endif 

