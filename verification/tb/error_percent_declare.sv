`ifdef ST_WIDTH_INF_4
reg [63:0] gol_yjp0  ;
reg [63:0] gol_yjp1  ;
reg [63:0] dut_yjp0  ;
reg [63:0] dut_yjp1  ;
`endif //ST_WIDTH_INF_4
`ifdef ST_WIDTH_INF_4
real gol_re_yjp0  ;
real gol_re_yjp1  ;
real gol_im_yjp0  ;
real gol_im_yjp1  ;
real dut_re_yjp0  ;
real dut_re_yjp1  ;
real dut_im_yjp0  ;
real dut_im_yjp1  ;
`endif //ST_WIDTH_INF_4
`ifdef ST_WIDTH_INF_4
real err_re_per0   ;    
real err_re_per1   ;    
real err_im_per0   ;    
real err_im_per1   ;    
`endif //ST_WIDTH_INF_4

`ifdef ST_WIDTH_INF_8
reg [63:0] gol_yjp0  ;
reg [63:0] gol_yjp1  ;
reg [63:0] dut_yjp0  ;
reg [63:0] dut_yjp1  ;
reg [63:0] gol_yjp2  ;
reg [63:0] gol_yjp3  ;
reg [63:0] dut_yjp2  ;
reg [63:0] dut_yjp3  ;

`endif //ST_WIDTH_INF_8
`ifdef ST_WIDTH_INF_8
real gol_re_yjp0  ;
real gol_re_yjp1  ;
real gol_im_yjp0  ;
real gol_im_yjp1  ;
real dut_re_yjp0  ;
real dut_re_yjp1  ;
real dut_im_yjp0  ;
real dut_im_yjp1  ;
//
real gol_re_yjp2  ;
real gol_re_yjp3  ;
real gol_im_yjp2  ;
real gol_im_yjp3  ;
real dut_re_yjp2  ;
real dut_re_yjp3  ;
real dut_im_yjp2  ;
real dut_im_yjp3  ;

`endif //ST_WIDTH_INF_8
`ifdef ST_WIDTH_INF_8
real err_re_per0   ;    
real err_re_per1   ;    
real err_im_per0   ;    
real err_im_per1   ;
real err_re_per2   ;    
real err_re_per3   ;    
real err_im_per2   ;    
real err_im_per3   ; 
`endif //ST_WIDTH_INF_4

`ifdef ST_WIDTH_INF_16
reg [63:0] gol_yjp0;
reg [63:0] dut_yjp0;
real gol_re_yjp0;
real dut_re_yjp0;
real gol_im_yjp0;
real dut_im_yjp0;

reg [63:0] gol_yjp1;
reg [63:0] dut_yjp1;
real gol_re_yjp1;
real dut_re_yjp1;
real gol_im_yjp1;
real dut_im_yjp1;

reg [63:0] gol_yjp2;
reg [63:0] dut_yjp2;
real gol_re_yjp2;
real dut_re_yjp2;
real gol_im_yjp2;
real dut_im_yjp2;

reg [63:0] gol_yjp3;
reg [63:0] dut_yjp3;
real gol_re_yjp3;
real dut_re_yjp3;
real gol_im_yjp3;
real dut_im_yjp3;

reg [63:0] gol_yjp4;
reg [63:0] dut_yjp4;
real gol_re_yjp4;
real dut_re_yjp4;
real gol_im_yjp4;
real dut_im_yjp4;

reg [63:0] gol_yjp5;
reg [63:0] dut_yjp5;
real gol_re_yjp5;
real dut_re_yjp5;
real gol_im_yjp5;
real dut_im_yjp5;

reg [63:0] gol_yjp6;
reg [63:0] dut_yjp6;
real gol_re_yjp6;
real dut_re_yjp6;
real gol_im_yjp6;
real dut_im_yjp6;

reg [63:0] gol_yjp7;
reg [63:0] dut_yjp7;
real gol_re_yjp7;
real dut_re_yjp7;
real gol_im_yjp7;
real dut_im_yjp7;

`endif 


`ifdef ST_WIDTH_INF_16
real err_re_per0   ;    
real err_im_per0   ;    

real err_re_per1   ;    
real err_im_per1   ;    

real err_re_per2   ;    
real err_im_per2   ;    

real err_re_per3   ;    
real err_im_per3   ;    

real err_re_per4   ;    
real err_im_per4   ;    

real err_re_per5   ;    
real err_im_per5   ;    

real err_re_per6   ;    
real err_im_per6   ;    

real err_re_per7   ;    
real err_im_per7   ;    

`endif



`ifdef ST_WIDTH_INF_512
reg [63:0] gol_yjp0;
reg [63:0] dut_yjp0;
real gol_re_yjp0;
real dut_re_yjp0;
real gol_im_yjp0;
real dut_im_yjp0;

reg [63:0] gol_yjp1;
reg [63:0] dut_yjp1;
real gol_re_yjp1;
real dut_re_yjp1;
real gol_im_yjp1;
real dut_im_yjp1;

reg [63:0] gol_yjp2;
reg [63:0] dut_yjp2;
real gol_re_yjp2;
real dut_re_yjp2;
real gol_im_yjp2;
real dut_im_yjp2;

reg [63:0] gol_yjp3;
reg [63:0] dut_yjp3;
real gol_re_yjp3;
real dut_re_yjp3;
real gol_im_yjp3;
real dut_im_yjp3;

reg [63:0] gol_yjp4;
reg [63:0] dut_yjp4;
real gol_re_yjp4;
real dut_re_yjp4;
real gol_im_yjp4;
real dut_im_yjp4;

reg [63:0] gol_yjp5;
reg [63:0] dut_yjp5;
real gol_re_yjp5;
real dut_re_yjp5;
real gol_im_yjp5;
real dut_im_yjp5;

reg [63:0] gol_yjp6;
reg [63:0] dut_yjp6;
real gol_re_yjp6;
real dut_re_yjp6;
real gol_im_yjp6;
real dut_im_yjp6;

reg [63:0] gol_yjp7;
reg [63:0] dut_yjp7;
real gol_re_yjp7;
real dut_re_yjp7;
real gol_im_yjp7;
real dut_im_yjp7;

reg [63:0] gol_yjp8;
reg [63:0] dut_yjp8;
real gol_re_yjp8;
real dut_re_yjp8;
real gol_im_yjp8;
real dut_im_yjp8;

reg [63:0] gol_yjp9;
reg [63:0] dut_yjp9;
real gol_re_yjp9;
real dut_re_yjp9;
real gol_im_yjp9;
real dut_im_yjp9;

reg [63:0] gol_yjp10;
reg [63:0] dut_yjp10;
real gol_re_yjp10;
real dut_re_yjp10;
real gol_im_yjp10;
real dut_im_yjp10;

reg [63:0] gol_yjp11;
reg [63:0] dut_yjp11;
real gol_re_yjp11;
real dut_re_yjp11;
real gol_im_yjp11;
real dut_im_yjp11;

reg [63:0] gol_yjp12;
reg [63:0] dut_yjp12;
real gol_re_yjp12;
real dut_re_yjp12;
real gol_im_yjp12;
real dut_im_yjp12;

reg [63:0] gol_yjp13;
reg [63:0] dut_yjp13;
real gol_re_yjp13;
real dut_re_yjp13;
real gol_im_yjp13;
real dut_im_yjp13;

reg [63:0] gol_yjp14;
reg [63:0] dut_yjp14;
real gol_re_yjp14;
real dut_re_yjp14;
real gol_im_yjp14;
real dut_im_yjp14;

reg [63:0] gol_yjp15;
reg [63:0] dut_yjp15;
real gol_re_yjp15;
real dut_re_yjp15;
real gol_im_yjp15;
real dut_im_yjp15;

reg [63:0] gol_yjp16;
reg [63:0] dut_yjp16;
real gol_re_yjp16;
real dut_re_yjp16;
real gol_im_yjp16;
real dut_im_yjp16;

reg [63:0] gol_yjp17;
reg [63:0] dut_yjp17;
real gol_re_yjp17;
real dut_re_yjp17;
real gol_im_yjp17;
real dut_im_yjp17;

reg [63:0] gol_yjp18;
reg [63:0] dut_yjp18;
real gol_re_yjp18;
real dut_re_yjp18;
real gol_im_yjp18;
real dut_im_yjp18;

reg [63:0] gol_yjp19;
reg [63:0] dut_yjp19;
real gol_re_yjp19;
real dut_re_yjp19;
real gol_im_yjp19;
real dut_im_yjp19;

reg [63:0] gol_yjp20;
reg [63:0] dut_yjp20;
real gol_re_yjp20;
real dut_re_yjp20;
real gol_im_yjp20;
real dut_im_yjp20;

reg [63:0] gol_yjp21;
reg [63:0] dut_yjp21;
real gol_re_yjp21;
real dut_re_yjp21;
real gol_im_yjp21;
real dut_im_yjp21;

reg [63:0] gol_yjp22;
reg [63:0] dut_yjp22;
real gol_re_yjp22;
real dut_re_yjp22;
real gol_im_yjp22;
real dut_im_yjp22;

reg [63:0] gol_yjp23;
reg [63:0] dut_yjp23;
real gol_re_yjp23;
real dut_re_yjp23;
real gol_im_yjp23;
real dut_im_yjp23;

reg [63:0] gol_yjp24;
reg [63:0] dut_yjp24;
real gol_re_yjp24;
real dut_re_yjp24;
real gol_im_yjp24;
real dut_im_yjp24;

reg [63:0] gol_yjp25;
reg [63:0] dut_yjp25;
real gol_re_yjp25;
real dut_re_yjp25;
real gol_im_yjp25;
real dut_im_yjp25;

reg [63:0] gol_yjp26;
reg [63:0] dut_yjp26;
real gol_re_yjp26;
real dut_re_yjp26;
real gol_im_yjp26;
real dut_im_yjp26;

reg [63:0] gol_yjp27;
reg [63:0] dut_yjp27;
real gol_re_yjp27;
real dut_re_yjp27;
real gol_im_yjp27;
real dut_im_yjp27;

reg [63:0] gol_yjp28;
reg [63:0] dut_yjp28;
real gol_re_yjp28;
real dut_re_yjp28;
real gol_im_yjp28;
real dut_im_yjp28;

reg [63:0] gol_yjp29;
reg [63:0] dut_yjp29;
real gol_re_yjp29;
real dut_re_yjp29;
real gol_im_yjp29;
real dut_im_yjp29;

reg [63:0] gol_yjp30;
reg [63:0] dut_yjp30;
real gol_re_yjp30;
real dut_re_yjp30;
real gol_im_yjp30;
real dut_im_yjp30;

reg [63:0] gol_yjp31;
reg [63:0] dut_yjp31;
real gol_re_yjp31;
real dut_re_yjp31;
real gol_im_yjp31;
real dut_im_yjp31;

reg [63:0] gol_yjp32;
reg [63:0] dut_yjp32;
real gol_re_yjp32;
real dut_re_yjp32;
real gol_im_yjp32;
real dut_im_yjp32;

reg [63:0] gol_yjp33;
reg [63:0] dut_yjp33;
real gol_re_yjp33;
real dut_re_yjp33;
real gol_im_yjp33;
real dut_im_yjp33;

reg [63:0] gol_yjp34;
reg [63:0] dut_yjp34;
real gol_re_yjp34;
real dut_re_yjp34;
real gol_im_yjp34;
real dut_im_yjp34;

reg [63:0] gol_yjp35;
reg [63:0] dut_yjp35;
real gol_re_yjp35;
real dut_re_yjp35;
real gol_im_yjp35;
real dut_im_yjp35;

reg [63:0] gol_yjp36;
reg [63:0] dut_yjp36;
real gol_re_yjp36;
real dut_re_yjp36;
real gol_im_yjp36;
real dut_im_yjp36;

reg [63:0] gol_yjp37;
reg [63:0] dut_yjp37;
real gol_re_yjp37;
real dut_re_yjp37;
real gol_im_yjp37;
real dut_im_yjp37;

reg [63:0] gol_yjp38;
reg [63:0] dut_yjp38;
real gol_re_yjp38;
real dut_re_yjp38;
real gol_im_yjp38;
real dut_im_yjp38;

reg [63:0] gol_yjp39;
reg [63:0] dut_yjp39;
real gol_re_yjp39;
real dut_re_yjp39;
real gol_im_yjp39;
real dut_im_yjp39;

reg [63:0] gol_yjp40;
reg [63:0] dut_yjp40;
real gol_re_yjp40;
real dut_re_yjp40;
real gol_im_yjp40;
real dut_im_yjp40;

reg [63:0] gol_yjp41;
reg [63:0] dut_yjp41;
real gol_re_yjp41;
real dut_re_yjp41;
real gol_im_yjp41;
real dut_im_yjp41;

reg [63:0] gol_yjp42;
reg [63:0] dut_yjp42;
real gol_re_yjp42;
real dut_re_yjp42;
real gol_im_yjp42;
real dut_im_yjp42;

reg [63:0] gol_yjp43;
reg [63:0] dut_yjp43;
real gol_re_yjp43;
real dut_re_yjp43;
real gol_im_yjp43;
real dut_im_yjp43;

reg [63:0] gol_yjp44;
reg [63:0] dut_yjp44;
real gol_re_yjp44;
real dut_re_yjp44;
real gol_im_yjp44;
real dut_im_yjp44;

reg [63:0] gol_yjp45;
reg [63:0] dut_yjp45;
real gol_re_yjp45;
real dut_re_yjp45;
real gol_im_yjp45;
real dut_im_yjp45;

reg [63:0] gol_yjp46;
reg [63:0] dut_yjp46;
real gol_re_yjp46;
real dut_re_yjp46;
real gol_im_yjp46;
real dut_im_yjp46;

reg [63:0] gol_yjp47;
reg [63:0] dut_yjp47;
real gol_re_yjp47;
real dut_re_yjp47;
real gol_im_yjp47;
real dut_im_yjp47;

reg [63:0] gol_yjp48;
reg [63:0] dut_yjp48;
real gol_re_yjp48;
real dut_re_yjp48;
real gol_im_yjp48;
real dut_im_yjp48;

reg [63:0] gol_yjp49;
reg [63:0] dut_yjp49;
real gol_re_yjp49;
real dut_re_yjp49;
real gol_im_yjp49;
real dut_im_yjp49;

reg [63:0] gol_yjp50;
reg [63:0] dut_yjp50;
real gol_re_yjp50;
real dut_re_yjp50;
real gol_im_yjp50;
real dut_im_yjp50;

reg [63:0] gol_yjp51;
reg [63:0] dut_yjp51;
real gol_re_yjp51;
real dut_re_yjp51;
real gol_im_yjp51;
real dut_im_yjp51;

reg [63:0] gol_yjp52;
reg [63:0] dut_yjp52;
real gol_re_yjp52;
real dut_re_yjp52;
real gol_im_yjp52;
real dut_im_yjp52;

reg [63:0] gol_yjp53;
reg [63:0] dut_yjp53;
real gol_re_yjp53;
real dut_re_yjp53;
real gol_im_yjp53;
real dut_im_yjp53;

reg [63:0] gol_yjp54;
reg [63:0] dut_yjp54;
real gol_re_yjp54;
real dut_re_yjp54;
real gol_im_yjp54;
real dut_im_yjp54;

reg [63:0] gol_yjp55;
reg [63:0] dut_yjp55;
real gol_re_yjp55;
real dut_re_yjp55;
real gol_im_yjp55;
real dut_im_yjp55;

reg [63:0] gol_yjp56;
reg [63:0] dut_yjp56;
real gol_re_yjp56;
real dut_re_yjp56;
real gol_im_yjp56;
real dut_im_yjp56;

reg [63:0] gol_yjp57;
reg [63:0] dut_yjp57;
real gol_re_yjp57;
real dut_re_yjp57;
real gol_im_yjp57;
real dut_im_yjp57;

reg [63:0] gol_yjp58;
reg [63:0] dut_yjp58;
real gol_re_yjp58;
real dut_re_yjp58;
real gol_im_yjp58;
real dut_im_yjp58;

reg [63:0] gol_yjp59;
reg [63:0] dut_yjp59;
real gol_re_yjp59;
real dut_re_yjp59;
real gol_im_yjp59;
real dut_im_yjp59;

reg [63:0] gol_yjp60;
reg [63:0] dut_yjp60;
real gol_re_yjp60;
real dut_re_yjp60;
real gol_im_yjp60;
real dut_im_yjp60;

reg [63:0] gol_yjp61;
reg [63:0] dut_yjp61;
real gol_re_yjp61;
real dut_re_yjp61;
real gol_im_yjp61;
real dut_im_yjp61;

reg [63:0] gol_yjp62;
reg [63:0] dut_yjp62;
real gol_re_yjp62;
real dut_re_yjp62;
real gol_im_yjp62;
real dut_im_yjp62;

reg [63:0] gol_yjp63;
reg [63:0] dut_yjp63;
real gol_re_yjp63;
real dut_re_yjp63;
real gol_im_yjp63;
real dut_im_yjp63;

reg [63:0] gol_yjp64;
reg [63:0] dut_yjp64;
real gol_re_yjp64;
real dut_re_yjp64;
real gol_im_yjp64;
real dut_im_yjp64;

reg [63:0] gol_yjp65;
reg [63:0] dut_yjp65;
real gol_re_yjp65;
real dut_re_yjp65;
real gol_im_yjp65;
real dut_im_yjp65;

reg [63:0] gol_yjp66;
reg [63:0] dut_yjp66;
real gol_re_yjp66;
real dut_re_yjp66;
real gol_im_yjp66;
real dut_im_yjp66;

reg [63:0] gol_yjp67;
reg [63:0] dut_yjp67;
real gol_re_yjp67;
real dut_re_yjp67;
real gol_im_yjp67;
real dut_im_yjp67;

reg [63:0] gol_yjp68;
reg [63:0] dut_yjp68;
real gol_re_yjp68;
real dut_re_yjp68;
real gol_im_yjp68;
real dut_im_yjp68;

reg [63:0] gol_yjp69;
reg [63:0] dut_yjp69;
real gol_re_yjp69;
real dut_re_yjp69;
real gol_im_yjp69;
real dut_im_yjp69;

reg [63:0] gol_yjp70;
reg [63:0] dut_yjp70;
real gol_re_yjp70;
real dut_re_yjp70;
real gol_im_yjp70;
real dut_im_yjp70;

reg [63:0] gol_yjp71;
reg [63:0] dut_yjp71;
real gol_re_yjp71;
real dut_re_yjp71;
real gol_im_yjp71;
real dut_im_yjp71;

reg [63:0] gol_yjp72;
reg [63:0] dut_yjp72;
real gol_re_yjp72;
real dut_re_yjp72;
real gol_im_yjp72;
real dut_im_yjp72;

reg [63:0] gol_yjp73;
reg [63:0] dut_yjp73;
real gol_re_yjp73;
real dut_re_yjp73;
real gol_im_yjp73;
real dut_im_yjp73;

reg [63:0] gol_yjp74;
reg [63:0] dut_yjp74;
real gol_re_yjp74;
real dut_re_yjp74;
real gol_im_yjp74;
real dut_im_yjp74;

reg [63:0] gol_yjp75;
reg [63:0] dut_yjp75;
real gol_re_yjp75;
real dut_re_yjp75;
real gol_im_yjp75;
real dut_im_yjp75;

reg [63:0] gol_yjp76;
reg [63:0] dut_yjp76;
real gol_re_yjp76;
real dut_re_yjp76;
real gol_im_yjp76;
real dut_im_yjp76;

reg [63:0] gol_yjp77;
reg [63:0] dut_yjp77;
real gol_re_yjp77;
real dut_re_yjp77;
real gol_im_yjp77;
real dut_im_yjp77;

reg [63:0] gol_yjp78;
reg [63:0] dut_yjp78;
real gol_re_yjp78;
real dut_re_yjp78;
real gol_im_yjp78;
real dut_im_yjp78;

reg [63:0] gol_yjp79;
reg [63:0] dut_yjp79;
real gol_re_yjp79;
real dut_re_yjp79;
real gol_im_yjp79;
real dut_im_yjp79;

reg [63:0] gol_yjp80;
reg [63:0] dut_yjp80;
real gol_re_yjp80;
real dut_re_yjp80;
real gol_im_yjp80;
real dut_im_yjp80;

reg [63:0] gol_yjp81;
reg [63:0] dut_yjp81;
real gol_re_yjp81;
real dut_re_yjp81;
real gol_im_yjp81;
real dut_im_yjp81;

reg [63:0] gol_yjp82;
reg [63:0] dut_yjp82;
real gol_re_yjp82;
real dut_re_yjp82;
real gol_im_yjp82;
real dut_im_yjp82;

reg [63:0] gol_yjp83;
reg [63:0] dut_yjp83;
real gol_re_yjp83;
real dut_re_yjp83;
real gol_im_yjp83;
real dut_im_yjp83;

reg [63:0] gol_yjp84;
reg [63:0] dut_yjp84;
real gol_re_yjp84;
real dut_re_yjp84;
real gol_im_yjp84;
real dut_im_yjp84;

reg [63:0] gol_yjp85;
reg [63:0] dut_yjp85;
real gol_re_yjp85;
real dut_re_yjp85;
real gol_im_yjp85;
real dut_im_yjp85;

reg [63:0] gol_yjp86;
reg [63:0] dut_yjp86;
real gol_re_yjp86;
real dut_re_yjp86;
real gol_im_yjp86;
real dut_im_yjp86;

reg [63:0] gol_yjp87;
reg [63:0] dut_yjp87;
real gol_re_yjp87;
real dut_re_yjp87;
real gol_im_yjp87;
real dut_im_yjp87;

reg [63:0] gol_yjp88;
reg [63:0] dut_yjp88;
real gol_re_yjp88;
real dut_re_yjp88;
real gol_im_yjp88;
real dut_im_yjp88;

reg [63:0] gol_yjp89;
reg [63:0] dut_yjp89;
real gol_re_yjp89;
real dut_re_yjp89;
real gol_im_yjp89;
real dut_im_yjp89;

reg [63:0] gol_yjp90;
reg [63:0] dut_yjp90;
real gol_re_yjp90;
real dut_re_yjp90;
real gol_im_yjp90;
real dut_im_yjp90;

reg [63:0] gol_yjp91;
reg [63:0] dut_yjp91;
real gol_re_yjp91;
real dut_re_yjp91;
real gol_im_yjp91;
real dut_im_yjp91;

reg [63:0] gol_yjp92;
reg [63:0] dut_yjp92;
real gol_re_yjp92;
real dut_re_yjp92;
real gol_im_yjp92;
real dut_im_yjp92;

reg [63:0] gol_yjp93;
reg [63:0] dut_yjp93;
real gol_re_yjp93;
real dut_re_yjp93;
real gol_im_yjp93;
real dut_im_yjp93;

reg [63:0] gol_yjp94;
reg [63:0] dut_yjp94;
real gol_re_yjp94;
real dut_re_yjp94;
real gol_im_yjp94;
real dut_im_yjp94;

reg [63:0] gol_yjp95;
reg [63:0] dut_yjp95;
real gol_re_yjp95;
real dut_re_yjp95;
real gol_im_yjp95;
real dut_im_yjp95;

reg [63:0] gol_yjp96;
reg [63:0] dut_yjp96;
real gol_re_yjp96;
real dut_re_yjp96;
real gol_im_yjp96;
real dut_im_yjp96;

reg [63:0] gol_yjp97;
reg [63:0] dut_yjp97;
real gol_re_yjp97;
real dut_re_yjp97;
real gol_im_yjp97;
real dut_im_yjp97;

reg [63:0] gol_yjp98;
reg [63:0] dut_yjp98;
real gol_re_yjp98;
real dut_re_yjp98;
real gol_im_yjp98;
real dut_im_yjp98;

reg [63:0] gol_yjp99;
reg [63:0] dut_yjp99;
real gol_re_yjp99;
real dut_re_yjp99;
real gol_im_yjp99;
real dut_im_yjp99;

reg [63:0] gol_yjp100;
reg [63:0] dut_yjp100;
real gol_re_yjp100;
real dut_re_yjp100;
real gol_im_yjp100;
real dut_im_yjp100;

reg [63:0] gol_yjp101;
reg [63:0] dut_yjp101;
real gol_re_yjp101;
real dut_re_yjp101;
real gol_im_yjp101;
real dut_im_yjp101;

reg [63:0] gol_yjp102;
reg [63:0] dut_yjp102;
real gol_re_yjp102;
real dut_re_yjp102;
real gol_im_yjp102;
real dut_im_yjp102;

reg [63:0] gol_yjp103;
reg [63:0] dut_yjp103;
real gol_re_yjp103;
real dut_re_yjp103;
real gol_im_yjp103;
real dut_im_yjp103;

reg [63:0] gol_yjp104;
reg [63:0] dut_yjp104;
real gol_re_yjp104;
real dut_re_yjp104;
real gol_im_yjp104;
real dut_im_yjp104;

reg [63:0] gol_yjp105;
reg [63:0] dut_yjp105;
real gol_re_yjp105;
real dut_re_yjp105;
real gol_im_yjp105;
real dut_im_yjp105;

reg [63:0] gol_yjp106;
reg [63:0] dut_yjp106;
real gol_re_yjp106;
real dut_re_yjp106;
real gol_im_yjp106;
real dut_im_yjp106;

reg [63:0] gol_yjp107;
reg [63:0] dut_yjp107;
real gol_re_yjp107;
real dut_re_yjp107;
real gol_im_yjp107;
real dut_im_yjp107;

reg [63:0] gol_yjp108;
reg [63:0] dut_yjp108;
real gol_re_yjp108;
real dut_re_yjp108;
real gol_im_yjp108;
real dut_im_yjp108;

reg [63:0] gol_yjp109;
reg [63:0] dut_yjp109;
real gol_re_yjp109;
real dut_re_yjp109;
real gol_im_yjp109;
real dut_im_yjp109;

reg [63:0] gol_yjp110;
reg [63:0] dut_yjp110;
real gol_re_yjp110;
real dut_re_yjp110;
real gol_im_yjp110;
real dut_im_yjp110;

reg [63:0] gol_yjp111;
reg [63:0] dut_yjp111;
real gol_re_yjp111;
real dut_re_yjp111;
real gol_im_yjp111;
real dut_im_yjp111;

reg [63:0] gol_yjp112;
reg [63:0] dut_yjp112;
real gol_re_yjp112;
real dut_re_yjp112;
real gol_im_yjp112;
real dut_im_yjp112;

reg [63:0] gol_yjp113;
reg [63:0] dut_yjp113;
real gol_re_yjp113;
real dut_re_yjp113;
real gol_im_yjp113;
real dut_im_yjp113;

reg [63:0] gol_yjp114;
reg [63:0] dut_yjp114;
real gol_re_yjp114;
real dut_re_yjp114;
real gol_im_yjp114;
real dut_im_yjp114;

reg [63:0] gol_yjp115;
reg [63:0] dut_yjp115;
real gol_re_yjp115;
real dut_re_yjp115;
real gol_im_yjp115;
real dut_im_yjp115;

reg [63:0] gol_yjp116;
reg [63:0] dut_yjp116;
real gol_re_yjp116;
real dut_re_yjp116;
real gol_im_yjp116;
real dut_im_yjp116;

reg [63:0] gol_yjp117;
reg [63:0] dut_yjp117;
real gol_re_yjp117;
real dut_re_yjp117;
real gol_im_yjp117;
real dut_im_yjp117;

reg [63:0] gol_yjp118;
reg [63:0] dut_yjp118;
real gol_re_yjp118;
real dut_re_yjp118;
real gol_im_yjp118;
real dut_im_yjp118;

reg [63:0] gol_yjp119;
reg [63:0] dut_yjp119;
real gol_re_yjp119;
real dut_re_yjp119;
real gol_im_yjp119;
real dut_im_yjp119;

reg [63:0] gol_yjp120;
reg [63:0] dut_yjp120;
real gol_re_yjp120;
real dut_re_yjp120;
real gol_im_yjp120;
real dut_im_yjp120;

reg [63:0] gol_yjp121;
reg [63:0] dut_yjp121;
real gol_re_yjp121;
real dut_re_yjp121;
real gol_im_yjp121;
real dut_im_yjp121;

reg [63:0] gol_yjp122;
reg [63:0] dut_yjp122;
real gol_re_yjp122;
real dut_re_yjp122;
real gol_im_yjp122;
real dut_im_yjp122;

reg [63:0] gol_yjp123;
reg [63:0] dut_yjp123;
real gol_re_yjp123;
real dut_re_yjp123;
real gol_im_yjp123;
real dut_im_yjp123;

reg [63:0] gol_yjp124;
reg [63:0] dut_yjp124;
real gol_re_yjp124;
real dut_re_yjp124;
real gol_im_yjp124;
real dut_im_yjp124;

reg [63:0] gol_yjp125;
reg [63:0] dut_yjp125;
real gol_re_yjp125;
real dut_re_yjp125;
real gol_im_yjp125;
real dut_im_yjp125;

reg [63:0] gol_yjp126;
reg [63:0] dut_yjp126;
real gol_re_yjp126;
real dut_re_yjp126;
real gol_im_yjp126;
real dut_im_yjp126;

reg [63:0] gol_yjp127;
reg [63:0] dut_yjp127;
real gol_re_yjp127;
real dut_re_yjp127;
real gol_im_yjp127;
real dut_im_yjp127;

reg [63:0] gol_yjp128;
reg [63:0] dut_yjp128;
real gol_re_yjp128;
real dut_re_yjp128;
real gol_im_yjp128;
real dut_im_yjp128;

reg [63:0] gol_yjp129;
reg [63:0] dut_yjp129;
real gol_re_yjp129;
real dut_re_yjp129;
real gol_im_yjp129;
real dut_im_yjp129;

reg [63:0] gol_yjp130;
reg [63:0] dut_yjp130;
real gol_re_yjp130;
real dut_re_yjp130;
real gol_im_yjp130;
real dut_im_yjp130;

reg [63:0] gol_yjp131;
reg [63:0] dut_yjp131;
real gol_re_yjp131;
real dut_re_yjp131;
real gol_im_yjp131;
real dut_im_yjp131;

reg [63:0] gol_yjp132;
reg [63:0] dut_yjp132;
real gol_re_yjp132;
real dut_re_yjp132;
real gol_im_yjp132;
real dut_im_yjp132;

reg [63:0] gol_yjp133;
reg [63:0] dut_yjp133;
real gol_re_yjp133;
real dut_re_yjp133;
real gol_im_yjp133;
real dut_im_yjp133;

reg [63:0] gol_yjp134;
reg [63:0] dut_yjp134;
real gol_re_yjp134;
real dut_re_yjp134;
real gol_im_yjp134;
real dut_im_yjp134;

reg [63:0] gol_yjp135;
reg [63:0] dut_yjp135;
real gol_re_yjp135;
real dut_re_yjp135;
real gol_im_yjp135;
real dut_im_yjp135;

reg [63:0] gol_yjp136;
reg [63:0] dut_yjp136;
real gol_re_yjp136;
real dut_re_yjp136;
real gol_im_yjp136;
real dut_im_yjp136;

reg [63:0] gol_yjp137;
reg [63:0] dut_yjp137;
real gol_re_yjp137;
real dut_re_yjp137;
real gol_im_yjp137;
real dut_im_yjp137;

reg [63:0] gol_yjp138;
reg [63:0] dut_yjp138;
real gol_re_yjp138;
real dut_re_yjp138;
real gol_im_yjp138;
real dut_im_yjp138;

reg [63:0] gol_yjp139;
reg [63:0] dut_yjp139;
real gol_re_yjp139;
real dut_re_yjp139;
real gol_im_yjp139;
real dut_im_yjp139;

reg [63:0] gol_yjp140;
reg [63:0] dut_yjp140;
real gol_re_yjp140;
real dut_re_yjp140;
real gol_im_yjp140;
real dut_im_yjp140;

reg [63:0] gol_yjp141;
reg [63:0] dut_yjp141;
real gol_re_yjp141;
real dut_re_yjp141;
real gol_im_yjp141;
real dut_im_yjp141;

reg [63:0] gol_yjp142;
reg [63:0] dut_yjp142;
real gol_re_yjp142;
real dut_re_yjp142;
real gol_im_yjp142;
real dut_im_yjp142;

reg [63:0] gol_yjp143;
reg [63:0] dut_yjp143;
real gol_re_yjp143;
real dut_re_yjp143;
real gol_im_yjp143;
real dut_im_yjp143;

reg [63:0] gol_yjp144;
reg [63:0] dut_yjp144;
real gol_re_yjp144;
real dut_re_yjp144;
real gol_im_yjp144;
real dut_im_yjp144;

reg [63:0] gol_yjp145;
reg [63:0] dut_yjp145;
real gol_re_yjp145;
real dut_re_yjp145;
real gol_im_yjp145;
real dut_im_yjp145;

reg [63:0] gol_yjp146;
reg [63:0] dut_yjp146;
real gol_re_yjp146;
real dut_re_yjp146;
real gol_im_yjp146;
real dut_im_yjp146;

reg [63:0] gol_yjp147;
reg [63:0] dut_yjp147;
real gol_re_yjp147;
real dut_re_yjp147;
real gol_im_yjp147;
real dut_im_yjp147;

reg [63:0] gol_yjp148;
reg [63:0] dut_yjp148;
real gol_re_yjp148;
real dut_re_yjp148;
real gol_im_yjp148;
real dut_im_yjp148;

reg [63:0] gol_yjp149;
reg [63:0] dut_yjp149;
real gol_re_yjp149;
real dut_re_yjp149;
real gol_im_yjp149;
real dut_im_yjp149;

reg [63:0] gol_yjp150;
reg [63:0] dut_yjp150;
real gol_re_yjp150;
real dut_re_yjp150;
real gol_im_yjp150;
real dut_im_yjp150;

reg [63:0] gol_yjp151;
reg [63:0] dut_yjp151;
real gol_re_yjp151;
real dut_re_yjp151;
real gol_im_yjp151;
real dut_im_yjp151;

reg [63:0] gol_yjp152;
reg [63:0] dut_yjp152;
real gol_re_yjp152;
real dut_re_yjp152;
real gol_im_yjp152;
real dut_im_yjp152;

reg [63:0] gol_yjp153;
reg [63:0] dut_yjp153;
real gol_re_yjp153;
real dut_re_yjp153;
real gol_im_yjp153;
real dut_im_yjp153;

reg [63:0] gol_yjp154;
reg [63:0] dut_yjp154;
real gol_re_yjp154;
real dut_re_yjp154;
real gol_im_yjp154;
real dut_im_yjp154;

reg [63:0] gol_yjp155;
reg [63:0] dut_yjp155;
real gol_re_yjp155;
real dut_re_yjp155;
real gol_im_yjp155;
real dut_im_yjp155;

reg [63:0] gol_yjp156;
reg [63:0] dut_yjp156;
real gol_re_yjp156;
real dut_re_yjp156;
real gol_im_yjp156;
real dut_im_yjp156;

reg [63:0] gol_yjp157;
reg [63:0] dut_yjp157;
real gol_re_yjp157;
real dut_re_yjp157;
real gol_im_yjp157;
real dut_im_yjp157;

reg [63:0] gol_yjp158;
reg [63:0] dut_yjp158;
real gol_re_yjp158;
real dut_re_yjp158;
real gol_im_yjp158;
real dut_im_yjp158;

reg [63:0] gol_yjp159;
reg [63:0] dut_yjp159;
real gol_re_yjp159;
real dut_re_yjp159;
real gol_im_yjp159;
real dut_im_yjp159;

reg [63:0] gol_yjp160;
reg [63:0] dut_yjp160;
real gol_re_yjp160;
real dut_re_yjp160;
real gol_im_yjp160;
real dut_im_yjp160;

reg [63:0] gol_yjp161;
reg [63:0] dut_yjp161;
real gol_re_yjp161;
real dut_re_yjp161;
real gol_im_yjp161;
real dut_im_yjp161;

reg [63:0] gol_yjp162;
reg [63:0] dut_yjp162;
real gol_re_yjp162;
real dut_re_yjp162;
real gol_im_yjp162;
real dut_im_yjp162;

reg [63:0] gol_yjp163;
reg [63:0] dut_yjp163;
real gol_re_yjp163;
real dut_re_yjp163;
real gol_im_yjp163;
real dut_im_yjp163;

reg [63:0] gol_yjp164;
reg [63:0] dut_yjp164;
real gol_re_yjp164;
real dut_re_yjp164;
real gol_im_yjp164;
real dut_im_yjp164;

reg [63:0] gol_yjp165;
reg [63:0] dut_yjp165;
real gol_re_yjp165;
real dut_re_yjp165;
real gol_im_yjp165;
real dut_im_yjp165;

reg [63:0] gol_yjp166;
reg [63:0] dut_yjp166;
real gol_re_yjp166;
real dut_re_yjp166;
real gol_im_yjp166;
real dut_im_yjp166;

reg [63:0] gol_yjp167;
reg [63:0] dut_yjp167;
real gol_re_yjp167;
real dut_re_yjp167;
real gol_im_yjp167;
real dut_im_yjp167;

reg [63:0] gol_yjp168;
reg [63:0] dut_yjp168;
real gol_re_yjp168;
real dut_re_yjp168;
real gol_im_yjp168;
real dut_im_yjp168;

reg [63:0] gol_yjp169;
reg [63:0] dut_yjp169;
real gol_re_yjp169;
real dut_re_yjp169;
real gol_im_yjp169;
real dut_im_yjp169;

reg [63:0] gol_yjp170;
reg [63:0] dut_yjp170;
real gol_re_yjp170;
real dut_re_yjp170;
real gol_im_yjp170;
real dut_im_yjp170;

reg [63:0] gol_yjp171;
reg [63:0] dut_yjp171;
real gol_re_yjp171;
real dut_re_yjp171;
real gol_im_yjp171;
real dut_im_yjp171;

reg [63:0] gol_yjp172;
reg [63:0] dut_yjp172;
real gol_re_yjp172;
real dut_re_yjp172;
real gol_im_yjp172;
real dut_im_yjp172;

reg [63:0] gol_yjp173;
reg [63:0] dut_yjp173;
real gol_re_yjp173;
real dut_re_yjp173;
real gol_im_yjp173;
real dut_im_yjp173;

reg [63:0] gol_yjp174;
reg [63:0] dut_yjp174;
real gol_re_yjp174;
real dut_re_yjp174;
real gol_im_yjp174;
real dut_im_yjp174;

reg [63:0] gol_yjp175;
reg [63:0] dut_yjp175;
real gol_re_yjp175;
real dut_re_yjp175;
real gol_im_yjp175;
real dut_im_yjp175;

reg [63:0] gol_yjp176;
reg [63:0] dut_yjp176;
real gol_re_yjp176;
real dut_re_yjp176;
real gol_im_yjp176;
real dut_im_yjp176;

reg [63:0] gol_yjp177;
reg [63:0] dut_yjp177;
real gol_re_yjp177;
real dut_re_yjp177;
real gol_im_yjp177;
real dut_im_yjp177;

reg [63:0] gol_yjp178;
reg [63:0] dut_yjp178;
real gol_re_yjp178;
real dut_re_yjp178;
real gol_im_yjp178;
real dut_im_yjp178;

reg [63:0] gol_yjp179;
reg [63:0] dut_yjp179;
real gol_re_yjp179;
real dut_re_yjp179;
real gol_im_yjp179;
real dut_im_yjp179;

reg [63:0] gol_yjp180;
reg [63:0] dut_yjp180;
real gol_re_yjp180;
real dut_re_yjp180;
real gol_im_yjp180;
real dut_im_yjp180;

reg [63:0] gol_yjp181;
reg [63:0] dut_yjp181;
real gol_re_yjp181;
real dut_re_yjp181;
real gol_im_yjp181;
real dut_im_yjp181;

reg [63:0] gol_yjp182;
reg [63:0] dut_yjp182;
real gol_re_yjp182;
real dut_re_yjp182;
real gol_im_yjp182;
real dut_im_yjp182;

reg [63:0] gol_yjp183;
reg [63:0] dut_yjp183;
real gol_re_yjp183;
real dut_re_yjp183;
real gol_im_yjp183;
real dut_im_yjp183;

reg [63:0] gol_yjp184;
reg [63:0] dut_yjp184;
real gol_re_yjp184;
real dut_re_yjp184;
real gol_im_yjp184;
real dut_im_yjp184;

reg [63:0] gol_yjp185;
reg [63:0] dut_yjp185;
real gol_re_yjp185;
real dut_re_yjp185;
real gol_im_yjp185;
real dut_im_yjp185;

reg [63:0] gol_yjp186;
reg [63:0] dut_yjp186;
real gol_re_yjp186;
real dut_re_yjp186;
real gol_im_yjp186;
real dut_im_yjp186;

reg [63:0] gol_yjp187;
reg [63:0] dut_yjp187;
real gol_re_yjp187;
real dut_re_yjp187;
real gol_im_yjp187;
real dut_im_yjp187;

reg [63:0] gol_yjp188;
reg [63:0] dut_yjp188;
real gol_re_yjp188;
real dut_re_yjp188;
real gol_im_yjp188;
real dut_im_yjp188;

reg [63:0] gol_yjp189;
reg [63:0] dut_yjp189;
real gol_re_yjp189;
real dut_re_yjp189;
real gol_im_yjp189;
real dut_im_yjp189;

reg [63:0] gol_yjp190;
reg [63:0] dut_yjp190;
real gol_re_yjp190;
real dut_re_yjp190;
real gol_im_yjp190;
real dut_im_yjp190;

reg [63:0] gol_yjp191;
reg [63:0] dut_yjp191;
real gol_re_yjp191;
real dut_re_yjp191;
real gol_im_yjp191;
real dut_im_yjp191;

reg [63:0] gol_yjp192;
reg [63:0] dut_yjp192;
real gol_re_yjp192;
real dut_re_yjp192;
real gol_im_yjp192;
real dut_im_yjp192;

reg [63:0] gol_yjp193;
reg [63:0] dut_yjp193;
real gol_re_yjp193;
real dut_re_yjp193;
real gol_im_yjp193;
real dut_im_yjp193;

reg [63:0] gol_yjp194;
reg [63:0] dut_yjp194;
real gol_re_yjp194;
real dut_re_yjp194;
real gol_im_yjp194;
real dut_im_yjp194;

reg [63:0] gol_yjp195;
reg [63:0] dut_yjp195;
real gol_re_yjp195;
real dut_re_yjp195;
real gol_im_yjp195;
real dut_im_yjp195;

reg [63:0] gol_yjp196;
reg [63:0] dut_yjp196;
real gol_re_yjp196;
real dut_re_yjp196;
real gol_im_yjp196;
real dut_im_yjp196;

reg [63:0] gol_yjp197;
reg [63:0] dut_yjp197;
real gol_re_yjp197;
real dut_re_yjp197;
real gol_im_yjp197;
real dut_im_yjp197;

reg [63:0] gol_yjp198;
reg [63:0] dut_yjp198;
real gol_re_yjp198;
real dut_re_yjp198;
real gol_im_yjp198;
real dut_im_yjp198;

reg [63:0] gol_yjp199;
reg [63:0] dut_yjp199;
real gol_re_yjp199;
real dut_re_yjp199;
real gol_im_yjp199;
real dut_im_yjp199;

reg [63:0] gol_yjp200;
reg [63:0] dut_yjp200;
real gol_re_yjp200;
real dut_re_yjp200;
real gol_im_yjp200;
real dut_im_yjp200;

reg [63:0] gol_yjp201;
reg [63:0] dut_yjp201;
real gol_re_yjp201;
real dut_re_yjp201;
real gol_im_yjp201;
real dut_im_yjp201;

reg [63:0] gol_yjp202;
reg [63:0] dut_yjp202;
real gol_re_yjp202;
real dut_re_yjp202;
real gol_im_yjp202;
real dut_im_yjp202;

reg [63:0] gol_yjp203;
reg [63:0] dut_yjp203;
real gol_re_yjp203;
real dut_re_yjp203;
real gol_im_yjp203;
real dut_im_yjp203;

reg [63:0] gol_yjp204;
reg [63:0] dut_yjp204;
real gol_re_yjp204;
real dut_re_yjp204;
real gol_im_yjp204;
real dut_im_yjp204;

reg [63:0] gol_yjp205;
reg [63:0] dut_yjp205;
real gol_re_yjp205;
real dut_re_yjp205;
real gol_im_yjp205;
real dut_im_yjp205;

reg [63:0] gol_yjp206;
reg [63:0] dut_yjp206;
real gol_re_yjp206;
real dut_re_yjp206;
real gol_im_yjp206;
real dut_im_yjp206;

reg [63:0] gol_yjp207;
reg [63:0] dut_yjp207;
real gol_re_yjp207;
real dut_re_yjp207;
real gol_im_yjp207;
real dut_im_yjp207;

reg [63:0] gol_yjp208;
reg [63:0] dut_yjp208;
real gol_re_yjp208;
real dut_re_yjp208;
real gol_im_yjp208;
real dut_im_yjp208;

reg [63:0] gol_yjp209;
reg [63:0] dut_yjp209;
real gol_re_yjp209;
real dut_re_yjp209;
real gol_im_yjp209;
real dut_im_yjp209;

reg [63:0] gol_yjp210;
reg [63:0] dut_yjp210;
real gol_re_yjp210;
real dut_re_yjp210;
real gol_im_yjp210;
real dut_im_yjp210;

reg [63:0] gol_yjp211;
reg [63:0] dut_yjp211;
real gol_re_yjp211;
real dut_re_yjp211;
real gol_im_yjp211;
real dut_im_yjp211;

reg [63:0] gol_yjp212;
reg [63:0] dut_yjp212;
real gol_re_yjp212;
real dut_re_yjp212;
real gol_im_yjp212;
real dut_im_yjp212;

reg [63:0] gol_yjp213;
reg [63:0] dut_yjp213;
real gol_re_yjp213;
real dut_re_yjp213;
real gol_im_yjp213;
real dut_im_yjp213;

reg [63:0] gol_yjp214;
reg [63:0] dut_yjp214;
real gol_re_yjp214;
real dut_re_yjp214;
real gol_im_yjp214;
real dut_im_yjp214;

reg [63:0] gol_yjp215;
reg [63:0] dut_yjp215;
real gol_re_yjp215;
real dut_re_yjp215;
real gol_im_yjp215;
real dut_im_yjp215;

reg [63:0] gol_yjp216;
reg [63:0] dut_yjp216;
real gol_re_yjp216;
real dut_re_yjp216;
real gol_im_yjp216;
real dut_im_yjp216;

reg [63:0] gol_yjp217;
reg [63:0] dut_yjp217;
real gol_re_yjp217;
real dut_re_yjp217;
real gol_im_yjp217;
real dut_im_yjp217;

reg [63:0] gol_yjp218;
reg [63:0] dut_yjp218;
real gol_re_yjp218;
real dut_re_yjp218;
real gol_im_yjp218;
real dut_im_yjp218;

reg [63:0] gol_yjp219;
reg [63:0] dut_yjp219;
real gol_re_yjp219;
real dut_re_yjp219;
real gol_im_yjp219;
real dut_im_yjp219;

reg [63:0] gol_yjp220;
reg [63:0] dut_yjp220;
real gol_re_yjp220;
real dut_re_yjp220;
real gol_im_yjp220;
real dut_im_yjp220;

reg [63:0] gol_yjp221;
reg [63:0] dut_yjp221;
real gol_re_yjp221;
real dut_re_yjp221;
real gol_im_yjp221;
real dut_im_yjp221;

reg [63:0] gol_yjp222;
reg [63:0] dut_yjp222;
real gol_re_yjp222;
real dut_re_yjp222;
real gol_im_yjp222;
real dut_im_yjp222;

reg [63:0] gol_yjp223;
reg [63:0] dut_yjp223;
real gol_re_yjp223;
real dut_re_yjp223;
real gol_im_yjp223;
real dut_im_yjp223;

reg [63:0] gol_yjp224;
reg [63:0] dut_yjp224;
real gol_re_yjp224;
real dut_re_yjp224;
real gol_im_yjp224;
real dut_im_yjp224;

reg [63:0] gol_yjp225;
reg [63:0] dut_yjp225;
real gol_re_yjp225;
real dut_re_yjp225;
real gol_im_yjp225;
real dut_im_yjp225;

reg [63:0] gol_yjp226;
reg [63:0] dut_yjp226;
real gol_re_yjp226;
real dut_re_yjp226;
real gol_im_yjp226;
real dut_im_yjp226;

reg [63:0] gol_yjp227;
reg [63:0] dut_yjp227;
real gol_re_yjp227;
real dut_re_yjp227;
real gol_im_yjp227;
real dut_im_yjp227;

reg [63:0] gol_yjp228;
reg [63:0] dut_yjp228;
real gol_re_yjp228;
real dut_re_yjp228;
real gol_im_yjp228;
real dut_im_yjp228;

reg [63:0] gol_yjp229;
reg [63:0] dut_yjp229;
real gol_re_yjp229;
real dut_re_yjp229;
real gol_im_yjp229;
real dut_im_yjp229;

reg [63:0] gol_yjp230;
reg [63:0] dut_yjp230;
real gol_re_yjp230;
real dut_re_yjp230;
real gol_im_yjp230;
real dut_im_yjp230;

reg [63:0] gol_yjp231;
reg [63:0] dut_yjp231;
real gol_re_yjp231;
real dut_re_yjp231;
real gol_im_yjp231;
real dut_im_yjp231;

reg [63:0] gol_yjp232;
reg [63:0] dut_yjp232;
real gol_re_yjp232;
real dut_re_yjp232;
real gol_im_yjp232;
real dut_im_yjp232;

reg [63:0] gol_yjp233;
reg [63:0] dut_yjp233;
real gol_re_yjp233;
real dut_re_yjp233;
real gol_im_yjp233;
real dut_im_yjp233;

reg [63:0] gol_yjp234;
reg [63:0] dut_yjp234;
real gol_re_yjp234;
real dut_re_yjp234;
real gol_im_yjp234;
real dut_im_yjp234;

reg [63:0] gol_yjp235;
reg [63:0] dut_yjp235;
real gol_re_yjp235;
real dut_re_yjp235;
real gol_im_yjp235;
real dut_im_yjp235;

reg [63:0] gol_yjp236;
reg [63:0] dut_yjp236;
real gol_re_yjp236;
real dut_re_yjp236;
real gol_im_yjp236;
real dut_im_yjp236;

reg [63:0] gol_yjp237;
reg [63:0] dut_yjp237;
real gol_re_yjp237;
real dut_re_yjp237;
real gol_im_yjp237;
real dut_im_yjp237;

reg [63:0] gol_yjp238;
reg [63:0] dut_yjp238;
real gol_re_yjp238;
real dut_re_yjp238;
real gol_im_yjp238;
real dut_im_yjp238;

reg [63:0] gol_yjp239;
reg [63:0] dut_yjp239;
real gol_re_yjp239;
real dut_re_yjp239;
real gol_im_yjp239;
real dut_im_yjp239;

reg [63:0] gol_yjp240;
reg [63:0] dut_yjp240;
real gol_re_yjp240;
real dut_re_yjp240;
real gol_im_yjp240;
real dut_im_yjp240;

reg [63:0] gol_yjp241;
reg [63:0] dut_yjp241;
real gol_re_yjp241;
real dut_re_yjp241;
real gol_im_yjp241;
real dut_im_yjp241;

reg [63:0] gol_yjp242;
reg [63:0] dut_yjp242;
real gol_re_yjp242;
real dut_re_yjp242;
real gol_im_yjp242;
real dut_im_yjp242;

reg [63:0] gol_yjp243;
reg [63:0] dut_yjp243;
real gol_re_yjp243;
real dut_re_yjp243;
real gol_im_yjp243;
real dut_im_yjp243;

reg [63:0] gol_yjp244;
reg [63:0] dut_yjp244;
real gol_re_yjp244;
real dut_re_yjp244;
real gol_im_yjp244;
real dut_im_yjp244;

reg [63:0] gol_yjp245;
reg [63:0] dut_yjp245;
real gol_re_yjp245;
real dut_re_yjp245;
real gol_im_yjp245;
real dut_im_yjp245;

reg [63:0] gol_yjp246;
reg [63:0] dut_yjp246;
real gol_re_yjp246;
real dut_re_yjp246;
real gol_im_yjp246;
real dut_im_yjp246;

reg [63:0] gol_yjp247;
reg [63:0] dut_yjp247;
real gol_re_yjp247;
real dut_re_yjp247;
real gol_im_yjp247;
real dut_im_yjp247;

reg [63:0] gol_yjp248;
reg [63:0] dut_yjp248;
real gol_re_yjp248;
real dut_re_yjp248;
real gol_im_yjp248;
real dut_im_yjp248;

reg [63:0] gol_yjp249;
reg [63:0] dut_yjp249;
real gol_re_yjp249;
real dut_re_yjp249;
real gol_im_yjp249;
real dut_im_yjp249;

reg [63:0] gol_yjp250;
reg [63:0] dut_yjp250;
real gol_re_yjp250;
real dut_re_yjp250;
real gol_im_yjp250;
real dut_im_yjp250;

reg [63:0] gol_yjp251;
reg [63:0] dut_yjp251;
real gol_re_yjp251;
real dut_re_yjp251;
real gol_im_yjp251;
real dut_im_yjp251;

reg [63:0] gol_yjp252;
reg [63:0] dut_yjp252;
real gol_re_yjp252;
real dut_re_yjp252;
real gol_im_yjp252;
real dut_im_yjp252;

reg [63:0] gol_yjp253;
reg [63:0] dut_yjp253;
real gol_re_yjp253;
real dut_re_yjp253;
real gol_im_yjp253;
real dut_im_yjp253;

reg [63:0] gol_yjp254;
reg [63:0] dut_yjp254;
real gol_re_yjp254;
real dut_re_yjp254;
real gol_im_yjp254;
real dut_im_yjp254;

reg [63:0] gol_yjp255;
reg [63:0] dut_yjp255;
real gol_re_yjp255;
real dut_re_yjp255;
real gol_im_yjp255;
real dut_im_yjp255;


`endif

`ifdef ST_WIDTH_INF_512
real err_re_per0   ;    
real err_im_per0   ;    

real err_re_per1   ;    
real err_im_per1   ;    

real err_re_per2   ;    
real err_im_per2   ;    

real err_re_per3   ;    
real err_im_per3   ;    

real err_re_per4   ;    
real err_im_per4   ;    

real err_re_per5   ;    
real err_im_per5   ;    

real err_re_per6   ;    
real err_im_per6   ;    

real err_re_per7   ;    
real err_im_per7   ;    

real err_re_per8   ;    
real err_im_per8   ;    

real err_re_per9   ;    
real err_im_per9   ;    

real err_re_per10   ;    
real err_im_per10   ;    

real err_re_per11   ;    
real err_im_per11   ;    

real err_re_per12   ;    
real err_im_per12   ;    

real err_re_per13   ;    
real err_im_per13   ;    

real err_re_per14   ;    
real err_im_per14   ;    

real err_re_per15   ;    
real err_im_per15   ;    

real err_re_per16   ;    
real err_im_per16   ;    

real err_re_per17   ;    
real err_im_per17   ;    

real err_re_per18   ;    
real err_im_per18   ;    

real err_re_per19   ;    
real err_im_per19   ;    

real err_re_per20   ;    
real err_im_per20   ;    

real err_re_per21   ;    
real err_im_per21   ;    

real err_re_per22   ;    
real err_im_per22   ;    

real err_re_per23   ;    
real err_im_per23   ;    

real err_re_per24   ;    
real err_im_per24   ;    

real err_re_per25   ;    
real err_im_per25   ;    

real err_re_per26   ;    
real err_im_per26   ;    

real err_re_per27   ;    
real err_im_per27   ;    

real err_re_per28   ;    
real err_im_per28   ;    

real err_re_per29   ;    
real err_im_per29   ;    

real err_re_per30   ;    
real err_im_per30   ;    

real err_re_per31   ;    
real err_im_per31   ;    

real err_re_per32   ;    
real err_im_per32   ;    

real err_re_per33   ;    
real err_im_per33   ;    

real err_re_per34   ;    
real err_im_per34   ;    

real err_re_per35   ;    
real err_im_per35   ;    

real err_re_per36   ;    
real err_im_per36   ;    

real err_re_per37   ;    
real err_im_per37   ;    

real err_re_per38   ;    
real err_im_per38   ;    

real err_re_per39   ;    
real err_im_per39   ;    

real err_re_per40   ;    
real err_im_per40   ;    

real err_re_per41   ;    
real err_im_per41   ;    

real err_re_per42   ;    
real err_im_per42   ;    

real err_re_per43   ;    
real err_im_per43   ;    

real err_re_per44   ;    
real err_im_per44   ;    

real err_re_per45   ;    
real err_im_per45   ;    

real err_re_per46   ;    
real err_im_per46   ;    

real err_re_per47   ;    
real err_im_per47   ;    

real err_re_per48   ;    
real err_im_per48   ;    

real err_re_per49   ;    
real err_im_per49   ;    

real err_re_per50   ;    
real err_im_per50   ;    

real err_re_per51   ;    
real err_im_per51   ;    

real err_re_per52   ;    
real err_im_per52   ;    

real err_re_per53   ;    
real err_im_per53   ;    

real err_re_per54   ;    
real err_im_per54   ;    

real err_re_per55   ;    
real err_im_per55   ;    

real err_re_per56   ;    
real err_im_per56   ;    

real err_re_per57   ;    
real err_im_per57   ;    

real err_re_per58   ;    
real err_im_per58   ;    

real err_re_per59   ;    
real err_im_per59   ;    

real err_re_per60   ;    
real err_im_per60   ;    

real err_re_per61   ;    
real err_im_per61   ;    

real err_re_per62   ;    
real err_im_per62   ;    

real err_re_per63   ;    
real err_im_per63   ;    

real err_re_per64   ;    
real err_im_per64   ;    

real err_re_per65   ;    
real err_im_per65   ;    

real err_re_per66   ;    
real err_im_per66   ;    

real err_re_per67   ;    
real err_im_per67   ;    

real err_re_per68   ;    
real err_im_per68   ;    

real err_re_per69   ;    
real err_im_per69   ;    

real err_re_per70   ;    
real err_im_per70   ;    

real err_re_per71   ;    
real err_im_per71   ;    

real err_re_per72   ;    
real err_im_per72   ;    

real err_re_per73   ;    
real err_im_per73   ;    

real err_re_per74   ;    
real err_im_per74   ;    

real err_re_per75   ;    
real err_im_per75   ;    

real err_re_per76   ;    
real err_im_per76   ;    

real err_re_per77   ;    
real err_im_per77   ;    

real err_re_per78   ;    
real err_im_per78   ;    

real err_re_per79   ;    
real err_im_per79   ;    

real err_re_per80   ;    
real err_im_per80   ;    

real err_re_per81   ;    
real err_im_per81   ;    

real err_re_per82   ;    
real err_im_per82   ;    

real err_re_per83   ;    
real err_im_per83   ;    

real err_re_per84   ;    
real err_im_per84   ;    

real err_re_per85   ;    
real err_im_per85   ;    

real err_re_per86   ;    
real err_im_per86   ;    

real err_re_per87   ;    
real err_im_per87   ;    

real err_re_per88   ;    
real err_im_per88   ;    

real err_re_per89   ;    
real err_im_per89   ;    

real err_re_per90   ;    
real err_im_per90   ;    

real err_re_per91   ;    
real err_im_per91   ;    

real err_re_per92   ;    
real err_im_per92   ;    

real err_re_per93   ;    
real err_im_per93   ;    

real err_re_per94   ;    
real err_im_per94   ;    

real err_re_per95   ;    
real err_im_per95   ;    

real err_re_per96   ;    
real err_im_per96   ;    

real err_re_per97   ;    
real err_im_per97   ;    

real err_re_per98   ;    
real err_im_per98   ;    

real err_re_per99   ;    
real err_im_per99   ;    

real err_re_per100   ;    
real err_im_per100   ;    

real err_re_per101   ;    
real err_im_per101   ;    

real err_re_per102   ;    
real err_im_per102   ;    

real err_re_per103   ;    
real err_im_per103   ;    

real err_re_per104   ;    
real err_im_per104   ;    

real err_re_per105   ;    
real err_im_per105   ;    

real err_re_per106   ;    
real err_im_per106   ;    

real err_re_per107   ;    
real err_im_per107   ;    

real err_re_per108   ;    
real err_im_per108   ;    

real err_re_per109   ;    
real err_im_per109   ;    

real err_re_per110   ;    
real err_im_per110   ;    

real err_re_per111   ;    
real err_im_per111   ;    

real err_re_per112   ;    
real err_im_per112   ;    

real err_re_per113   ;    
real err_im_per113   ;    

real err_re_per114   ;    
real err_im_per114   ;    

real err_re_per115   ;    
real err_im_per115   ;    

real err_re_per116   ;    
real err_im_per116   ;    

real err_re_per117   ;    
real err_im_per117   ;    

real err_re_per118   ;    
real err_im_per118   ;    

real err_re_per119   ;    
real err_im_per119   ;    

real err_re_per120   ;    
real err_im_per120   ;    

real err_re_per121   ;    
real err_im_per121   ;    

real err_re_per122   ;    
real err_im_per122   ;    

real err_re_per123   ;    
real err_im_per123   ;    

real err_re_per124   ;    
real err_im_per124   ;    

real err_re_per125   ;    
real err_im_per125   ;    

real err_re_per126   ;    
real err_im_per126   ;    

real err_re_per127   ;    
real err_im_per127   ;    

real err_re_per128   ;    
real err_im_per128   ;    

real err_re_per129   ;    
real err_im_per129   ;    

real err_re_per130   ;    
real err_im_per130   ;    

real err_re_per131   ;    
real err_im_per131   ;    

real err_re_per132   ;    
real err_im_per132   ;    

real err_re_per133   ;    
real err_im_per133   ;    

real err_re_per134   ;    
real err_im_per134   ;    

real err_re_per135   ;    
real err_im_per135   ;    

real err_re_per136   ;    
real err_im_per136   ;    

real err_re_per137   ;    
real err_im_per137   ;    

real err_re_per138   ;    
real err_im_per138   ;    

real err_re_per139   ;    
real err_im_per139   ;    

real err_re_per140   ;    
real err_im_per140   ;    

real err_re_per141   ;    
real err_im_per141   ;    

real err_re_per142   ;    
real err_im_per142   ;    

real err_re_per143   ;    
real err_im_per143   ;    

real err_re_per144   ;    
real err_im_per144   ;    

real err_re_per145   ;    
real err_im_per145   ;    

real err_re_per146   ;    
real err_im_per146   ;    

real err_re_per147   ;    
real err_im_per147   ;    

real err_re_per148   ;    
real err_im_per148   ;    

real err_re_per149   ;    
real err_im_per149   ;    

real err_re_per150   ;    
real err_im_per150   ;    

real err_re_per151   ;    
real err_im_per151   ;    

real err_re_per152   ;    
real err_im_per152   ;    

real err_re_per153   ;    
real err_im_per153   ;    

real err_re_per154   ;    
real err_im_per154   ;    

real err_re_per155   ;    
real err_im_per155   ;    

real err_re_per156   ;    
real err_im_per156   ;    

real err_re_per157   ;    
real err_im_per157   ;    

real err_re_per158   ;    
real err_im_per158   ;    

real err_re_per159   ;    
real err_im_per159   ;    

real err_re_per160   ;    
real err_im_per160   ;    

real err_re_per161   ;    
real err_im_per161   ;    

real err_re_per162   ;    
real err_im_per162   ;    

real err_re_per163   ;    
real err_im_per163   ;    

real err_re_per164   ;    
real err_im_per164   ;    

real err_re_per165   ;    
real err_im_per165   ;    

real err_re_per166   ;    
real err_im_per166   ;    

real err_re_per167   ;    
real err_im_per167   ;    

real err_re_per168   ;    
real err_im_per168   ;    

real err_re_per169   ;    
real err_im_per169   ;    

real err_re_per170   ;    
real err_im_per170   ;    

real err_re_per171   ;    
real err_im_per171   ;    

real err_re_per172   ;    
real err_im_per172   ;    

real err_re_per173   ;    
real err_im_per173   ;    

real err_re_per174   ;    
real err_im_per174   ;    

real err_re_per175   ;    
real err_im_per175   ;    

real err_re_per176   ;    
real err_im_per176   ;    

real err_re_per177   ;    
real err_im_per177   ;    

real err_re_per178   ;    
real err_im_per178   ;    

real err_re_per179   ;    
real err_im_per179   ;    

real err_re_per180   ;    
real err_im_per180   ;    

real err_re_per181   ;    
real err_im_per181   ;    

real err_re_per182   ;    
real err_im_per182   ;    

real err_re_per183   ;    
real err_im_per183   ;    

real err_re_per184   ;    
real err_im_per184   ;    

real err_re_per185   ;    
real err_im_per185   ;    

real err_re_per186   ;    
real err_im_per186   ;    

real err_re_per187   ;    
real err_im_per187   ;    

real err_re_per188   ;    
real err_im_per188   ;    

real err_re_per189   ;    
real err_im_per189   ;    

real err_re_per190   ;    
real err_im_per190   ;    

real err_re_per191   ;    
real err_im_per191   ;    

real err_re_per192   ;    
real err_im_per192   ;    

real err_re_per193   ;    
real err_im_per193   ;    

real err_re_per194   ;    
real err_im_per194   ;    

real err_re_per195   ;    
real err_im_per195   ;    

real err_re_per196   ;    
real err_im_per196   ;    

real err_re_per197   ;    
real err_im_per197   ;    

real err_re_per198   ;    
real err_im_per198   ;    

real err_re_per199   ;    
real err_im_per199   ;    

real err_re_per200   ;    
real err_im_per200   ;    

real err_re_per201   ;    
real err_im_per201   ;    

real err_re_per202   ;    
real err_im_per202   ;    

real err_re_per203   ;    
real err_im_per203   ;    

real err_re_per204   ;    
real err_im_per204   ;    

real err_re_per205   ;    
real err_im_per205   ;    

real err_re_per206   ;    
real err_im_per206   ;    

real err_re_per207   ;    
real err_im_per207   ;    

real err_re_per208   ;    
real err_im_per208   ;    

real err_re_per209   ;    
real err_im_per209   ;    

real err_re_per210   ;    
real err_im_per210   ;    

real err_re_per211   ;    
real err_im_per211   ;    

real err_re_per212   ;    
real err_im_per212   ;    

real err_re_per213   ;    
real err_im_per213   ;    

real err_re_per214   ;    
real err_im_per214   ;    

real err_re_per215   ;    
real err_im_per215   ;    

real err_re_per216   ;    
real err_im_per216   ;    

real err_re_per217   ;    
real err_im_per217   ;    

real err_re_per218   ;    
real err_im_per218   ;    

real err_re_per219   ;    
real err_im_per219   ;    

real err_re_per220   ;    
real err_im_per220   ;    

real err_re_per221   ;    
real err_im_per221   ;    

real err_re_per222   ;    
real err_im_per222   ;    

real err_re_per223   ;    
real err_im_per223   ;    

real err_re_per224   ;    
real err_im_per224   ;    

real err_re_per225   ;    
real err_im_per225   ;    

real err_re_per226   ;    
real err_im_per226   ;    

real err_re_per227   ;    
real err_im_per227   ;    

real err_re_per228   ;    
real err_im_per228   ;    

real err_re_per229   ;    
real err_im_per229   ;    

real err_re_per230   ;    
real err_im_per230   ;    

real err_re_per231   ;    
real err_im_per231   ;    

real err_re_per232   ;    
real err_im_per232   ;    

real err_re_per233   ;    
real err_im_per233   ;    

real err_re_per234   ;    
real err_im_per234   ;    

real err_re_per235   ;    
real err_im_per235   ;    

real err_re_per236   ;    
real err_im_per236   ;    

real err_re_per237   ;    
real err_im_per237   ;    

real err_re_per238   ;    
real err_im_per238   ;    

real err_re_per239   ;    
real err_im_per239   ;    

real err_re_per240   ;    
real err_im_per240   ;    

real err_re_per241   ;    
real err_im_per241   ;    

real err_re_per242   ;    
real err_im_per242   ;    

real err_re_per243   ;    
real err_im_per243   ;    

real err_re_per244   ;    
real err_im_per244   ;    

real err_re_per245   ;    
real err_im_per245   ;    

real err_re_per246   ;    
real err_im_per246   ;    

real err_re_per247   ;    
real err_im_per247   ;    

real err_re_per248   ;    
real err_im_per248   ;    

real err_re_per249   ;    
real err_im_per249   ;    

real err_re_per250   ;    
real err_im_per250   ;    

real err_re_per251   ;    
real err_im_per251   ;    

real err_re_per252   ;    
real err_im_per252   ;    

real err_re_per253   ;    
real err_im_per253   ;    

real err_re_per254   ;    
real err_im_per254   ;    

real err_re_per255   ;    
real err_im_per255   ;    

`endif



