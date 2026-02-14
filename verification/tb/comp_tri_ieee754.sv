`ifdef ST_WIDTH_INF_4
$fwrite(tri_report_ieee754, "===============================%d Column Results    ======================================\n",col_index);
if(col_index>=0  ) $fwrite(tri_report_ieee754, "Item0   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per0  , gol_yjp0[63:32]  , dut_yjp0[63:32]  );
if(col_index>=0  ) $fwrite(tri_report_ieee754, "Item0   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per0  , gol_yjp0[31: 0]  , dut_yjp0[31: 0]  );
if(col_index>=1  ) $fwrite(tri_report_ieee754, "Item1   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per1  , gol_yjp1[63:32]  , dut_yjp1[63:32]  );
if(col_index>=1  ) $fwrite(tri_report_ieee754, "Item1   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per1  , gol_yjp1[31: 0]  , dut_yjp1[31: 0]  );
`endif

`ifdef ST_WIDTH_INF_8
$fwrite(tri_report_ieee754, "===============================%d Column Results    ======================================\n",col_index);
if(col_index>=0  ) $fwrite(tri_report_ieee754, "Item0   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per0  , gol_yjp0[63:32]  , dut_yjp0[63:32]  );
if(col_index>=0  ) $fwrite(tri_report_ieee754, "Item0   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per0  , gol_yjp0[31: 0]  , dut_yjp0[31: 0]  );
if(col_index>=1  ) $fwrite(tri_report_ieee754, "Item1   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per1  , gol_yjp1[63:32]  , dut_yjp1[63:32]  );
if(col_index>=1  ) $fwrite(tri_report_ieee754, "Item1   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per1  , gol_yjp1[31: 0]  , dut_yjp1[31: 0]  );
//
if(col_index>=2  ) $fwrite(tri_report_ieee754, "Item2   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per2  , gol_yjp2[63:32]  , dut_yjp2[63:32]  );
if(col_index>=2  ) $fwrite(tri_report_ieee754, "Item2   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per2  , gol_yjp2[31: 0]  , dut_yjp2[31: 0]  );
if(col_index>=3  ) $fwrite(tri_report_ieee754, "Item3   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per3  , gol_yjp3[63:32]  , dut_yjp3[63:32]  );
if(col_index>=3  ) $fwrite(tri_report_ieee754, "Item3   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per3  , gol_yjp3[31: 0]  , dut_yjp3[31: 0]  );


`endif


`ifdef ST_WIDTH_INF_16
if(col_index>=0) $fwrite(tri_report_ieee754, "Item0   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per0, gol_yjp0[63:32], dut_yjp0[63:32]);
if(col_index>=0) $fwrite(tri_report_ieee754, "Item0   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per0, gol_yjp0[31:0], dut_yjp0[31:0]);

if(col_index>=1) $fwrite(tri_report_ieee754, "Item1   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per1, gol_yjp1[63:32], dut_yjp1[63:32]);
if(col_index>=1) $fwrite(tri_report_ieee754, "Item1   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per1, gol_yjp1[31:0], dut_yjp1[31:0]);

if(col_index>=2) $fwrite(tri_report_ieee754, "Item2   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per2, gol_yjp2[63:32], dut_yjp2[63:32]);
if(col_index>=2) $fwrite(tri_report_ieee754, "Item2   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per2, gol_yjp2[31:0], dut_yjp2[31:0]);

if(col_index>=3) $fwrite(tri_report_ieee754, "Item3   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per3, gol_yjp3[63:32], dut_yjp3[63:32]);
if(col_index>=3) $fwrite(tri_report_ieee754, "Item3   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per3, gol_yjp3[31:0], dut_yjp3[31:0]);

if(col_index>=4) $fwrite(tri_report_ieee754, "Item4   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per4, gol_yjp4[63:32], dut_yjp4[63:32]);
if(col_index>=4) $fwrite(tri_report_ieee754, "Item4   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per4, gol_yjp4[31:0], dut_yjp4[31:0]);

if(col_index>=5) $fwrite(tri_report_ieee754, "Item5   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per5, gol_yjp5[63:32], dut_yjp5[63:32]);
if(col_index>=5) $fwrite(tri_report_ieee754, "Item5   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per5, gol_yjp5[31:0], dut_yjp5[31:0]);

if(col_index>=6) $fwrite(tri_report_ieee754, "Item6   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per6, gol_yjp6[63:32], dut_yjp6[63:32]);
if(col_index>=6) $fwrite(tri_report_ieee754, "Item6   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per6, gol_yjp6[31:0], dut_yjp6[31:0]);

if(col_index>=7) $fwrite(tri_report_ieee754, "Item7   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per7, gol_yjp7[63:32], dut_yjp7[63:32]);
if(col_index>=7) $fwrite(tri_report_ieee754, "Item7   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per7, gol_yjp7[31:0], dut_yjp7[31:0]);

`endif






`ifdef ST_WIDTH_INF_512
if(col_index>=0) $fwrite(tri_report_ieee754, "Item0   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per0, gol_yjp0[63:32], dut_yjp0[63:32]);
if(col_index>=0) $fwrite(tri_report_ieee754, "Item0   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per0, gol_yjp0[31:0], dut_yjp0[31:0]);

if(col_index>=1) $fwrite(tri_report_ieee754, "Item1   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per1, gol_yjp1[63:32], dut_yjp1[63:32]);
if(col_index>=1) $fwrite(tri_report_ieee754, "Item1   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per1, gol_yjp1[31:0], dut_yjp1[31:0]);

if(col_index>=2) $fwrite(tri_report_ieee754, "Item2   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per2, gol_yjp2[63:32], dut_yjp2[63:32]);
if(col_index>=2) $fwrite(tri_report_ieee754, "Item2   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per2, gol_yjp2[31:0], dut_yjp2[31:0]);

if(col_index>=3) $fwrite(tri_report_ieee754, "Item3   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per3, gol_yjp3[63:32], dut_yjp3[63:32]);
if(col_index>=3) $fwrite(tri_report_ieee754, "Item3   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per3, gol_yjp3[31:0], dut_yjp3[31:0]);

if(col_index>=4) $fwrite(tri_report_ieee754, "Item4   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per4, gol_yjp4[63:32], dut_yjp4[63:32]);
if(col_index>=4) $fwrite(tri_report_ieee754, "Item4   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per4, gol_yjp4[31:0], dut_yjp4[31:0]);

if(col_index>=5) $fwrite(tri_report_ieee754, "Item5   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per5, gol_yjp5[63:32], dut_yjp5[63:32]);
if(col_index>=5) $fwrite(tri_report_ieee754, "Item5   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per5, gol_yjp5[31:0], dut_yjp5[31:0]);

if(col_index>=6) $fwrite(tri_report_ieee754, "Item6   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per6, gol_yjp6[63:32], dut_yjp6[63:32]);
if(col_index>=6) $fwrite(tri_report_ieee754, "Item6   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per6, gol_yjp6[31:0], dut_yjp6[31:0]);

if(col_index>=7) $fwrite(tri_report_ieee754, "Item7   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per7, gol_yjp7[63:32], dut_yjp7[63:32]);
if(col_index>=7) $fwrite(tri_report_ieee754, "Item7   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per7, gol_yjp7[31:0], dut_yjp7[31:0]);

if(col_index>=8) $fwrite(tri_report_ieee754, "Item8   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per8, gol_yjp8[63:32], dut_yjp8[63:32]);
if(col_index>=8) $fwrite(tri_report_ieee754, "Item8   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per8, gol_yjp8[31:0], dut_yjp8[31:0]);

if(col_index>=9) $fwrite(tri_report_ieee754, "Item9   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per9, gol_yjp9[63:32], dut_yjp9[63:32]);
if(col_index>=9) $fwrite(tri_report_ieee754, "Item9   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per9, gol_yjp9[31:0], dut_yjp9[31:0]);

if(col_index>=10) $fwrite(tri_report_ieee754, "Item10   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per10, gol_yjp10[63:32], dut_yjp10[63:32]);
if(col_index>=10) $fwrite(tri_report_ieee754, "Item10   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per10, gol_yjp10[31:0], dut_yjp10[31:0]);

if(col_index>=11) $fwrite(tri_report_ieee754, "Item11   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per11, gol_yjp11[63:32], dut_yjp11[63:32]);
if(col_index>=11) $fwrite(tri_report_ieee754, "Item11   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per11, gol_yjp11[31:0], dut_yjp11[31:0]);

if(col_index>=12) $fwrite(tri_report_ieee754, "Item12   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per12, gol_yjp12[63:32], dut_yjp12[63:32]);
if(col_index>=12) $fwrite(tri_report_ieee754, "Item12   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per12, gol_yjp12[31:0], dut_yjp12[31:0]);

if(col_index>=13) $fwrite(tri_report_ieee754, "Item13   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per13, gol_yjp13[63:32], dut_yjp13[63:32]);
if(col_index>=13) $fwrite(tri_report_ieee754, "Item13   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per13, gol_yjp13[31:0], dut_yjp13[31:0]);

if(col_index>=14) $fwrite(tri_report_ieee754, "Item14   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per14, gol_yjp14[63:32], dut_yjp14[63:32]);
if(col_index>=14) $fwrite(tri_report_ieee754, "Item14   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per14, gol_yjp14[31:0], dut_yjp14[31:0]);

if(col_index>=15) $fwrite(tri_report_ieee754, "Item15   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per15, gol_yjp15[63:32], dut_yjp15[63:32]);
if(col_index>=15) $fwrite(tri_report_ieee754, "Item15   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per15, gol_yjp15[31:0], dut_yjp15[31:0]);

if(col_index>=16) $fwrite(tri_report_ieee754, "Item16   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per16, gol_yjp16[63:32], dut_yjp16[63:32]);
if(col_index>=16) $fwrite(tri_report_ieee754, "Item16   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per16, gol_yjp16[31:0], dut_yjp16[31:0]);

if(col_index>=17) $fwrite(tri_report_ieee754, "Item17   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per17, gol_yjp17[63:32], dut_yjp17[63:32]);
if(col_index>=17) $fwrite(tri_report_ieee754, "Item17   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per17, gol_yjp17[31:0], dut_yjp17[31:0]);

if(col_index>=18) $fwrite(tri_report_ieee754, "Item18   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per18, gol_yjp18[63:32], dut_yjp18[63:32]);
if(col_index>=18) $fwrite(tri_report_ieee754, "Item18   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per18, gol_yjp18[31:0], dut_yjp18[31:0]);

if(col_index>=19) $fwrite(tri_report_ieee754, "Item19   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per19, gol_yjp19[63:32], dut_yjp19[63:32]);
if(col_index>=19) $fwrite(tri_report_ieee754, "Item19   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per19, gol_yjp19[31:0], dut_yjp19[31:0]);

if(col_index>=20) $fwrite(tri_report_ieee754, "Item20   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per20, gol_yjp20[63:32], dut_yjp20[63:32]);
if(col_index>=20) $fwrite(tri_report_ieee754, "Item20   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per20, gol_yjp20[31:0], dut_yjp20[31:0]);

if(col_index>=21) $fwrite(tri_report_ieee754, "Item21   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per21, gol_yjp21[63:32], dut_yjp21[63:32]);
if(col_index>=21) $fwrite(tri_report_ieee754, "Item21   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per21, gol_yjp21[31:0], dut_yjp21[31:0]);

if(col_index>=22) $fwrite(tri_report_ieee754, "Item22   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per22, gol_yjp22[63:32], dut_yjp22[63:32]);
if(col_index>=22) $fwrite(tri_report_ieee754, "Item22   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per22, gol_yjp22[31:0], dut_yjp22[31:0]);

if(col_index>=23) $fwrite(tri_report_ieee754, "Item23   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per23, gol_yjp23[63:32], dut_yjp23[63:32]);
if(col_index>=23) $fwrite(tri_report_ieee754, "Item23   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per23, gol_yjp23[31:0], dut_yjp23[31:0]);

if(col_index>=24) $fwrite(tri_report_ieee754, "Item24   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per24, gol_yjp24[63:32], dut_yjp24[63:32]);
if(col_index>=24) $fwrite(tri_report_ieee754, "Item24   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per24, gol_yjp24[31:0], dut_yjp24[31:0]);

if(col_index>=25) $fwrite(tri_report_ieee754, "Item25   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per25, gol_yjp25[63:32], dut_yjp25[63:32]);
if(col_index>=25) $fwrite(tri_report_ieee754, "Item25   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per25, gol_yjp25[31:0], dut_yjp25[31:0]);

if(col_index>=26) $fwrite(tri_report_ieee754, "Item26   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per26, gol_yjp26[63:32], dut_yjp26[63:32]);
if(col_index>=26) $fwrite(tri_report_ieee754, "Item26   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per26, gol_yjp26[31:0], dut_yjp26[31:0]);

if(col_index>=27) $fwrite(tri_report_ieee754, "Item27   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per27, gol_yjp27[63:32], dut_yjp27[63:32]);
if(col_index>=27) $fwrite(tri_report_ieee754, "Item27   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per27, gol_yjp27[31:0], dut_yjp27[31:0]);

if(col_index>=28) $fwrite(tri_report_ieee754, "Item28   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per28, gol_yjp28[63:32], dut_yjp28[63:32]);
if(col_index>=28) $fwrite(tri_report_ieee754, "Item28   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per28, gol_yjp28[31:0], dut_yjp28[31:0]);

if(col_index>=29) $fwrite(tri_report_ieee754, "Item29   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per29, gol_yjp29[63:32], dut_yjp29[63:32]);
if(col_index>=29) $fwrite(tri_report_ieee754, "Item29   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per29, gol_yjp29[31:0], dut_yjp29[31:0]);

if(col_index>=30) $fwrite(tri_report_ieee754, "Item30   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per30, gol_yjp30[63:32], dut_yjp30[63:32]);
if(col_index>=30) $fwrite(tri_report_ieee754, "Item30   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per30, gol_yjp30[31:0], dut_yjp30[31:0]);

if(col_index>=31) $fwrite(tri_report_ieee754, "Item31   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per31, gol_yjp31[63:32], dut_yjp31[63:32]);
if(col_index>=31) $fwrite(tri_report_ieee754, "Item31   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per31, gol_yjp31[31:0], dut_yjp31[31:0]);

if(col_index>=32) $fwrite(tri_report_ieee754, "Item32   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per32, gol_yjp32[63:32], dut_yjp32[63:32]);
if(col_index>=32) $fwrite(tri_report_ieee754, "Item32   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per32, gol_yjp32[31:0], dut_yjp32[31:0]);

if(col_index>=33) $fwrite(tri_report_ieee754, "Item33   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per33, gol_yjp33[63:32], dut_yjp33[63:32]);
if(col_index>=33) $fwrite(tri_report_ieee754, "Item33   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per33, gol_yjp33[31:0], dut_yjp33[31:0]);

if(col_index>=34) $fwrite(tri_report_ieee754, "Item34   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per34, gol_yjp34[63:32], dut_yjp34[63:32]);
if(col_index>=34) $fwrite(tri_report_ieee754, "Item34   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per34, gol_yjp34[31:0], dut_yjp34[31:0]);

if(col_index>=35) $fwrite(tri_report_ieee754, "Item35   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per35, gol_yjp35[63:32], dut_yjp35[63:32]);
if(col_index>=35) $fwrite(tri_report_ieee754, "Item35   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per35, gol_yjp35[31:0], dut_yjp35[31:0]);

if(col_index>=36) $fwrite(tri_report_ieee754, "Item36   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per36, gol_yjp36[63:32], dut_yjp36[63:32]);
if(col_index>=36) $fwrite(tri_report_ieee754, "Item36   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per36, gol_yjp36[31:0], dut_yjp36[31:0]);

if(col_index>=37) $fwrite(tri_report_ieee754, "Item37   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per37, gol_yjp37[63:32], dut_yjp37[63:32]);
if(col_index>=37) $fwrite(tri_report_ieee754, "Item37   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per37, gol_yjp37[31:0], dut_yjp37[31:0]);

if(col_index>=38) $fwrite(tri_report_ieee754, "Item38   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per38, gol_yjp38[63:32], dut_yjp38[63:32]);
if(col_index>=38) $fwrite(tri_report_ieee754, "Item38   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per38, gol_yjp38[31:0], dut_yjp38[31:0]);

if(col_index>=39) $fwrite(tri_report_ieee754, "Item39   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per39, gol_yjp39[63:32], dut_yjp39[63:32]);
if(col_index>=39) $fwrite(tri_report_ieee754, "Item39   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per39, gol_yjp39[31:0], dut_yjp39[31:0]);

if(col_index>=40) $fwrite(tri_report_ieee754, "Item40   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per40, gol_yjp40[63:32], dut_yjp40[63:32]);
if(col_index>=40) $fwrite(tri_report_ieee754, "Item40   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per40, gol_yjp40[31:0], dut_yjp40[31:0]);

if(col_index>=41) $fwrite(tri_report_ieee754, "Item41   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per41, gol_yjp41[63:32], dut_yjp41[63:32]);
if(col_index>=41) $fwrite(tri_report_ieee754, "Item41   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per41, gol_yjp41[31:0], dut_yjp41[31:0]);

if(col_index>=42) $fwrite(tri_report_ieee754, "Item42   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per42, gol_yjp42[63:32], dut_yjp42[63:32]);
if(col_index>=42) $fwrite(tri_report_ieee754, "Item42   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per42, gol_yjp42[31:0], dut_yjp42[31:0]);

if(col_index>=43) $fwrite(tri_report_ieee754, "Item43   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per43, gol_yjp43[63:32], dut_yjp43[63:32]);
if(col_index>=43) $fwrite(tri_report_ieee754, "Item43   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per43, gol_yjp43[31:0], dut_yjp43[31:0]);

if(col_index>=44) $fwrite(tri_report_ieee754, "Item44   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per44, gol_yjp44[63:32], dut_yjp44[63:32]);
if(col_index>=44) $fwrite(tri_report_ieee754, "Item44   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per44, gol_yjp44[31:0], dut_yjp44[31:0]);

if(col_index>=45) $fwrite(tri_report_ieee754, "Item45   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per45, gol_yjp45[63:32], dut_yjp45[63:32]);
if(col_index>=45) $fwrite(tri_report_ieee754, "Item45   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per45, gol_yjp45[31:0], dut_yjp45[31:0]);

if(col_index>=46) $fwrite(tri_report_ieee754, "Item46   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per46, gol_yjp46[63:32], dut_yjp46[63:32]);
if(col_index>=46) $fwrite(tri_report_ieee754, "Item46   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per46, gol_yjp46[31:0], dut_yjp46[31:0]);

if(col_index>=47) $fwrite(tri_report_ieee754, "Item47   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per47, gol_yjp47[63:32], dut_yjp47[63:32]);
if(col_index>=47) $fwrite(tri_report_ieee754, "Item47   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per47, gol_yjp47[31:0], dut_yjp47[31:0]);

if(col_index>=48) $fwrite(tri_report_ieee754, "Item48   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per48, gol_yjp48[63:32], dut_yjp48[63:32]);
if(col_index>=48) $fwrite(tri_report_ieee754, "Item48   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per48, gol_yjp48[31:0], dut_yjp48[31:0]);

if(col_index>=49) $fwrite(tri_report_ieee754, "Item49   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per49, gol_yjp49[63:32], dut_yjp49[63:32]);
if(col_index>=49) $fwrite(tri_report_ieee754, "Item49   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per49, gol_yjp49[31:0], dut_yjp49[31:0]);

if(col_index>=50) $fwrite(tri_report_ieee754, "Item50   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per50, gol_yjp50[63:32], dut_yjp50[63:32]);
if(col_index>=50) $fwrite(tri_report_ieee754, "Item50   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per50, gol_yjp50[31:0], dut_yjp50[31:0]);

if(col_index>=51) $fwrite(tri_report_ieee754, "Item51   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per51, gol_yjp51[63:32], dut_yjp51[63:32]);
if(col_index>=51) $fwrite(tri_report_ieee754, "Item51   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per51, gol_yjp51[31:0], dut_yjp51[31:0]);

if(col_index>=52) $fwrite(tri_report_ieee754, "Item52   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per52, gol_yjp52[63:32], dut_yjp52[63:32]);
if(col_index>=52) $fwrite(tri_report_ieee754, "Item52   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per52, gol_yjp52[31:0], dut_yjp52[31:0]);

if(col_index>=53) $fwrite(tri_report_ieee754, "Item53   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per53, gol_yjp53[63:32], dut_yjp53[63:32]);
if(col_index>=53) $fwrite(tri_report_ieee754, "Item53   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per53, gol_yjp53[31:0], dut_yjp53[31:0]);

if(col_index>=54) $fwrite(tri_report_ieee754, "Item54   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per54, gol_yjp54[63:32], dut_yjp54[63:32]);
if(col_index>=54) $fwrite(tri_report_ieee754, "Item54   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per54, gol_yjp54[31:0], dut_yjp54[31:0]);

if(col_index>=55) $fwrite(tri_report_ieee754, "Item55   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per55, gol_yjp55[63:32], dut_yjp55[63:32]);
if(col_index>=55) $fwrite(tri_report_ieee754, "Item55   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per55, gol_yjp55[31:0], dut_yjp55[31:0]);

if(col_index>=56) $fwrite(tri_report_ieee754, "Item56   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per56, gol_yjp56[63:32], dut_yjp56[63:32]);
if(col_index>=56) $fwrite(tri_report_ieee754, "Item56   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per56, gol_yjp56[31:0], dut_yjp56[31:0]);

if(col_index>=57) $fwrite(tri_report_ieee754, "Item57   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per57, gol_yjp57[63:32], dut_yjp57[63:32]);
if(col_index>=57) $fwrite(tri_report_ieee754, "Item57   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per57, gol_yjp57[31:0], dut_yjp57[31:0]);

if(col_index>=58) $fwrite(tri_report_ieee754, "Item58   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per58, gol_yjp58[63:32], dut_yjp58[63:32]);
if(col_index>=58) $fwrite(tri_report_ieee754, "Item58   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per58, gol_yjp58[31:0], dut_yjp58[31:0]);

if(col_index>=59) $fwrite(tri_report_ieee754, "Item59   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per59, gol_yjp59[63:32], dut_yjp59[63:32]);
if(col_index>=59) $fwrite(tri_report_ieee754, "Item59   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per59, gol_yjp59[31:0], dut_yjp59[31:0]);

if(col_index>=60) $fwrite(tri_report_ieee754, "Item60   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per60, gol_yjp60[63:32], dut_yjp60[63:32]);
if(col_index>=60) $fwrite(tri_report_ieee754, "Item60   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per60, gol_yjp60[31:0], dut_yjp60[31:0]);

if(col_index>=61) $fwrite(tri_report_ieee754, "Item61   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per61, gol_yjp61[63:32], dut_yjp61[63:32]);
if(col_index>=61) $fwrite(tri_report_ieee754, "Item61   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per61, gol_yjp61[31:0], dut_yjp61[31:0]);

if(col_index>=62) $fwrite(tri_report_ieee754, "Item62   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per62, gol_yjp62[63:32], dut_yjp62[63:32]);
if(col_index>=62) $fwrite(tri_report_ieee754, "Item62   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per62, gol_yjp62[31:0], dut_yjp62[31:0]);

if(col_index>=63) $fwrite(tri_report_ieee754, "Item63   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per63, gol_yjp63[63:32], dut_yjp63[63:32]);
if(col_index>=63) $fwrite(tri_report_ieee754, "Item63   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per63, gol_yjp63[31:0], dut_yjp63[31:0]);

if(col_index>=64) $fwrite(tri_report_ieee754, "Item64   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per64, gol_yjp64[63:32], dut_yjp64[63:32]);
if(col_index>=64) $fwrite(tri_report_ieee754, "Item64   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per64, gol_yjp64[31:0], dut_yjp64[31:0]);

if(col_index>=65) $fwrite(tri_report_ieee754, "Item65   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per65, gol_yjp65[63:32], dut_yjp65[63:32]);
if(col_index>=65) $fwrite(tri_report_ieee754, "Item65   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per65, gol_yjp65[31:0], dut_yjp65[31:0]);

if(col_index>=66) $fwrite(tri_report_ieee754, "Item66   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per66, gol_yjp66[63:32], dut_yjp66[63:32]);
if(col_index>=66) $fwrite(tri_report_ieee754, "Item66   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per66, gol_yjp66[31:0], dut_yjp66[31:0]);

if(col_index>=67) $fwrite(tri_report_ieee754, "Item67   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per67, gol_yjp67[63:32], dut_yjp67[63:32]);
if(col_index>=67) $fwrite(tri_report_ieee754, "Item67   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per67, gol_yjp67[31:0], dut_yjp67[31:0]);

if(col_index>=68) $fwrite(tri_report_ieee754, "Item68   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per68, gol_yjp68[63:32], dut_yjp68[63:32]);
if(col_index>=68) $fwrite(tri_report_ieee754, "Item68   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per68, gol_yjp68[31:0], dut_yjp68[31:0]);

if(col_index>=69) $fwrite(tri_report_ieee754, "Item69   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per69, gol_yjp69[63:32], dut_yjp69[63:32]);
if(col_index>=69) $fwrite(tri_report_ieee754, "Item69   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per69, gol_yjp69[31:0], dut_yjp69[31:0]);

if(col_index>=70) $fwrite(tri_report_ieee754, "Item70   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per70, gol_yjp70[63:32], dut_yjp70[63:32]);
if(col_index>=70) $fwrite(tri_report_ieee754, "Item70   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per70, gol_yjp70[31:0], dut_yjp70[31:0]);

if(col_index>=71) $fwrite(tri_report_ieee754, "Item71   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per71, gol_yjp71[63:32], dut_yjp71[63:32]);
if(col_index>=71) $fwrite(tri_report_ieee754, "Item71   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per71, gol_yjp71[31:0], dut_yjp71[31:0]);

if(col_index>=72) $fwrite(tri_report_ieee754, "Item72   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per72, gol_yjp72[63:32], dut_yjp72[63:32]);
if(col_index>=72) $fwrite(tri_report_ieee754, "Item72   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per72, gol_yjp72[31:0], dut_yjp72[31:0]);

if(col_index>=73) $fwrite(tri_report_ieee754, "Item73   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per73, gol_yjp73[63:32], dut_yjp73[63:32]);
if(col_index>=73) $fwrite(tri_report_ieee754, "Item73   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per73, gol_yjp73[31:0], dut_yjp73[31:0]);

if(col_index>=74) $fwrite(tri_report_ieee754, "Item74   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per74, gol_yjp74[63:32], dut_yjp74[63:32]);
if(col_index>=74) $fwrite(tri_report_ieee754, "Item74   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per74, gol_yjp74[31:0], dut_yjp74[31:0]);

if(col_index>=75) $fwrite(tri_report_ieee754, "Item75   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per75, gol_yjp75[63:32], dut_yjp75[63:32]);
if(col_index>=75) $fwrite(tri_report_ieee754, "Item75   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per75, gol_yjp75[31:0], dut_yjp75[31:0]);

if(col_index>=76) $fwrite(tri_report_ieee754, "Item76   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per76, gol_yjp76[63:32], dut_yjp76[63:32]);
if(col_index>=76) $fwrite(tri_report_ieee754, "Item76   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per76, gol_yjp76[31:0], dut_yjp76[31:0]);

if(col_index>=77) $fwrite(tri_report_ieee754, "Item77   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per77, gol_yjp77[63:32], dut_yjp77[63:32]);
if(col_index>=77) $fwrite(tri_report_ieee754, "Item77   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per77, gol_yjp77[31:0], dut_yjp77[31:0]);

if(col_index>=78) $fwrite(tri_report_ieee754, "Item78   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per78, gol_yjp78[63:32], dut_yjp78[63:32]);
if(col_index>=78) $fwrite(tri_report_ieee754, "Item78   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per78, gol_yjp78[31:0], dut_yjp78[31:0]);

if(col_index>=79) $fwrite(tri_report_ieee754, "Item79   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per79, gol_yjp79[63:32], dut_yjp79[63:32]);
if(col_index>=79) $fwrite(tri_report_ieee754, "Item79   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per79, gol_yjp79[31:0], dut_yjp79[31:0]);

if(col_index>=80) $fwrite(tri_report_ieee754, "Item80   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per80, gol_yjp80[63:32], dut_yjp80[63:32]);
if(col_index>=80) $fwrite(tri_report_ieee754, "Item80   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per80, gol_yjp80[31:0], dut_yjp80[31:0]);

if(col_index>=81) $fwrite(tri_report_ieee754, "Item81   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per81, gol_yjp81[63:32], dut_yjp81[63:32]);
if(col_index>=81) $fwrite(tri_report_ieee754, "Item81   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per81, gol_yjp81[31:0], dut_yjp81[31:0]);

if(col_index>=82) $fwrite(tri_report_ieee754, "Item82   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per82, gol_yjp82[63:32], dut_yjp82[63:32]);
if(col_index>=82) $fwrite(tri_report_ieee754, "Item82   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per82, gol_yjp82[31:0], dut_yjp82[31:0]);

if(col_index>=83) $fwrite(tri_report_ieee754, "Item83   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per83, gol_yjp83[63:32], dut_yjp83[63:32]);
if(col_index>=83) $fwrite(tri_report_ieee754, "Item83   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per83, gol_yjp83[31:0], dut_yjp83[31:0]);

if(col_index>=84) $fwrite(tri_report_ieee754, "Item84   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per84, gol_yjp84[63:32], dut_yjp84[63:32]);
if(col_index>=84) $fwrite(tri_report_ieee754, "Item84   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per84, gol_yjp84[31:0], dut_yjp84[31:0]);

if(col_index>=85) $fwrite(tri_report_ieee754, "Item85   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per85, gol_yjp85[63:32], dut_yjp85[63:32]);
if(col_index>=85) $fwrite(tri_report_ieee754, "Item85   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per85, gol_yjp85[31:0], dut_yjp85[31:0]);

if(col_index>=86) $fwrite(tri_report_ieee754, "Item86   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per86, gol_yjp86[63:32], dut_yjp86[63:32]);
if(col_index>=86) $fwrite(tri_report_ieee754, "Item86   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per86, gol_yjp86[31:0], dut_yjp86[31:0]);

if(col_index>=87) $fwrite(tri_report_ieee754, "Item87   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per87, gol_yjp87[63:32], dut_yjp87[63:32]);
if(col_index>=87) $fwrite(tri_report_ieee754, "Item87   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per87, gol_yjp87[31:0], dut_yjp87[31:0]);

if(col_index>=88) $fwrite(tri_report_ieee754, "Item88   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per88, gol_yjp88[63:32], dut_yjp88[63:32]);
if(col_index>=88) $fwrite(tri_report_ieee754, "Item88   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per88, gol_yjp88[31:0], dut_yjp88[31:0]);

if(col_index>=89) $fwrite(tri_report_ieee754, "Item89   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per89, gol_yjp89[63:32], dut_yjp89[63:32]);
if(col_index>=89) $fwrite(tri_report_ieee754, "Item89   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per89, gol_yjp89[31:0], dut_yjp89[31:0]);

if(col_index>=90) $fwrite(tri_report_ieee754, "Item90   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per90, gol_yjp90[63:32], dut_yjp90[63:32]);
if(col_index>=90) $fwrite(tri_report_ieee754, "Item90   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per90, gol_yjp90[31:0], dut_yjp90[31:0]);

if(col_index>=91) $fwrite(tri_report_ieee754, "Item91   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per91, gol_yjp91[63:32], dut_yjp91[63:32]);
if(col_index>=91) $fwrite(tri_report_ieee754, "Item91   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per91, gol_yjp91[31:0], dut_yjp91[31:0]);

if(col_index>=92) $fwrite(tri_report_ieee754, "Item92   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per92, gol_yjp92[63:32], dut_yjp92[63:32]);
if(col_index>=92) $fwrite(tri_report_ieee754, "Item92   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per92, gol_yjp92[31:0], dut_yjp92[31:0]);

if(col_index>=93) $fwrite(tri_report_ieee754, "Item93   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per93, gol_yjp93[63:32], dut_yjp93[63:32]);
if(col_index>=93) $fwrite(tri_report_ieee754, "Item93   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per93, gol_yjp93[31:0], dut_yjp93[31:0]);

if(col_index>=94) $fwrite(tri_report_ieee754, "Item94   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per94, gol_yjp94[63:32], dut_yjp94[63:32]);
if(col_index>=94) $fwrite(tri_report_ieee754, "Item94   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per94, gol_yjp94[31:0], dut_yjp94[31:0]);

if(col_index>=95) $fwrite(tri_report_ieee754, "Item95   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per95, gol_yjp95[63:32], dut_yjp95[63:32]);
if(col_index>=95) $fwrite(tri_report_ieee754, "Item95   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per95, gol_yjp95[31:0], dut_yjp95[31:0]);

if(col_index>=96) $fwrite(tri_report_ieee754, "Item96   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per96, gol_yjp96[63:32], dut_yjp96[63:32]);
if(col_index>=96) $fwrite(tri_report_ieee754, "Item96   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per96, gol_yjp96[31:0], dut_yjp96[31:0]);

if(col_index>=97) $fwrite(tri_report_ieee754, "Item97   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per97, gol_yjp97[63:32], dut_yjp97[63:32]);
if(col_index>=97) $fwrite(tri_report_ieee754, "Item97   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per97, gol_yjp97[31:0], dut_yjp97[31:0]);

if(col_index>=98) $fwrite(tri_report_ieee754, "Item98   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per98, gol_yjp98[63:32], dut_yjp98[63:32]);
if(col_index>=98) $fwrite(tri_report_ieee754, "Item98   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per98, gol_yjp98[31:0], dut_yjp98[31:0]);

if(col_index>=99) $fwrite(tri_report_ieee754, "Item99   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per99, gol_yjp99[63:32], dut_yjp99[63:32]);
if(col_index>=99) $fwrite(tri_report_ieee754, "Item99   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per99, gol_yjp99[31:0], dut_yjp99[31:0]);

if(col_index>=100) $fwrite(tri_report_ieee754, "Item100   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per100, gol_yjp100[63:32], dut_yjp100[63:32]);
if(col_index>=100) $fwrite(tri_report_ieee754, "Item100   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per100, gol_yjp100[31:0], dut_yjp100[31:0]);

if(col_index>=101) $fwrite(tri_report_ieee754, "Item101   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per101, gol_yjp101[63:32], dut_yjp101[63:32]);
if(col_index>=101) $fwrite(tri_report_ieee754, "Item101   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per101, gol_yjp101[31:0], dut_yjp101[31:0]);

if(col_index>=102) $fwrite(tri_report_ieee754, "Item102   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per102, gol_yjp102[63:32], dut_yjp102[63:32]);
if(col_index>=102) $fwrite(tri_report_ieee754, "Item102   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per102, gol_yjp102[31:0], dut_yjp102[31:0]);

if(col_index>=103) $fwrite(tri_report_ieee754, "Item103   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per103, gol_yjp103[63:32], dut_yjp103[63:32]);
if(col_index>=103) $fwrite(tri_report_ieee754, "Item103   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per103, gol_yjp103[31:0], dut_yjp103[31:0]);

if(col_index>=104) $fwrite(tri_report_ieee754, "Item104   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per104, gol_yjp104[63:32], dut_yjp104[63:32]);
if(col_index>=104) $fwrite(tri_report_ieee754, "Item104   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per104, gol_yjp104[31:0], dut_yjp104[31:0]);

if(col_index>=105) $fwrite(tri_report_ieee754, "Item105   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per105, gol_yjp105[63:32], dut_yjp105[63:32]);
if(col_index>=105) $fwrite(tri_report_ieee754, "Item105   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per105, gol_yjp105[31:0], dut_yjp105[31:0]);

if(col_index>=106) $fwrite(tri_report_ieee754, "Item106   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per106, gol_yjp106[63:32], dut_yjp106[63:32]);
if(col_index>=106) $fwrite(tri_report_ieee754, "Item106   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per106, gol_yjp106[31:0], dut_yjp106[31:0]);

if(col_index>=107) $fwrite(tri_report_ieee754, "Item107   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per107, gol_yjp107[63:32], dut_yjp107[63:32]);
if(col_index>=107) $fwrite(tri_report_ieee754, "Item107   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per107, gol_yjp107[31:0], dut_yjp107[31:0]);

if(col_index>=108) $fwrite(tri_report_ieee754, "Item108   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per108, gol_yjp108[63:32], dut_yjp108[63:32]);
if(col_index>=108) $fwrite(tri_report_ieee754, "Item108   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per108, gol_yjp108[31:0], dut_yjp108[31:0]);

if(col_index>=109) $fwrite(tri_report_ieee754, "Item109   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per109, gol_yjp109[63:32], dut_yjp109[63:32]);
if(col_index>=109) $fwrite(tri_report_ieee754, "Item109   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per109, gol_yjp109[31:0], dut_yjp109[31:0]);

if(col_index>=110) $fwrite(tri_report_ieee754, "Item110   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per110, gol_yjp110[63:32], dut_yjp110[63:32]);
if(col_index>=110) $fwrite(tri_report_ieee754, "Item110   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per110, gol_yjp110[31:0], dut_yjp110[31:0]);

if(col_index>=111) $fwrite(tri_report_ieee754, "Item111   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per111, gol_yjp111[63:32], dut_yjp111[63:32]);
if(col_index>=111) $fwrite(tri_report_ieee754, "Item111   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per111, gol_yjp111[31:0], dut_yjp111[31:0]);

if(col_index>=112) $fwrite(tri_report_ieee754, "Item112   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per112, gol_yjp112[63:32], dut_yjp112[63:32]);
if(col_index>=112) $fwrite(tri_report_ieee754, "Item112   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per112, gol_yjp112[31:0], dut_yjp112[31:0]);

if(col_index>=113) $fwrite(tri_report_ieee754, "Item113   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per113, gol_yjp113[63:32], dut_yjp113[63:32]);
if(col_index>=113) $fwrite(tri_report_ieee754, "Item113   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per113, gol_yjp113[31:0], dut_yjp113[31:0]);

if(col_index>=114) $fwrite(tri_report_ieee754, "Item114   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per114, gol_yjp114[63:32], dut_yjp114[63:32]);
if(col_index>=114) $fwrite(tri_report_ieee754, "Item114   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per114, gol_yjp114[31:0], dut_yjp114[31:0]);

if(col_index>=115) $fwrite(tri_report_ieee754, "Item115   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per115, gol_yjp115[63:32], dut_yjp115[63:32]);
if(col_index>=115) $fwrite(tri_report_ieee754, "Item115   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per115, gol_yjp115[31:0], dut_yjp115[31:0]);

if(col_index>=116) $fwrite(tri_report_ieee754, "Item116   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per116, gol_yjp116[63:32], dut_yjp116[63:32]);
if(col_index>=116) $fwrite(tri_report_ieee754, "Item116   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per116, gol_yjp116[31:0], dut_yjp116[31:0]);

if(col_index>=117) $fwrite(tri_report_ieee754, "Item117   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per117, gol_yjp117[63:32], dut_yjp117[63:32]);
if(col_index>=117) $fwrite(tri_report_ieee754, "Item117   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per117, gol_yjp117[31:0], dut_yjp117[31:0]);

if(col_index>=118) $fwrite(tri_report_ieee754, "Item118   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per118, gol_yjp118[63:32], dut_yjp118[63:32]);
if(col_index>=118) $fwrite(tri_report_ieee754, "Item118   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per118, gol_yjp118[31:0], dut_yjp118[31:0]);

if(col_index>=119) $fwrite(tri_report_ieee754, "Item119   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per119, gol_yjp119[63:32], dut_yjp119[63:32]);
if(col_index>=119) $fwrite(tri_report_ieee754, "Item119   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per119, gol_yjp119[31:0], dut_yjp119[31:0]);

if(col_index>=120) $fwrite(tri_report_ieee754, "Item120   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per120, gol_yjp120[63:32], dut_yjp120[63:32]);
if(col_index>=120) $fwrite(tri_report_ieee754, "Item120   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per120, gol_yjp120[31:0], dut_yjp120[31:0]);

if(col_index>=121) $fwrite(tri_report_ieee754, "Item121   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per121, gol_yjp121[63:32], dut_yjp121[63:32]);
if(col_index>=121) $fwrite(tri_report_ieee754, "Item121   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per121, gol_yjp121[31:0], dut_yjp121[31:0]);

if(col_index>=122) $fwrite(tri_report_ieee754, "Item122   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per122, gol_yjp122[63:32], dut_yjp122[63:32]);
if(col_index>=122) $fwrite(tri_report_ieee754, "Item122   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per122, gol_yjp122[31:0], dut_yjp122[31:0]);

if(col_index>=123) $fwrite(tri_report_ieee754, "Item123   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per123, gol_yjp123[63:32], dut_yjp123[63:32]);
if(col_index>=123) $fwrite(tri_report_ieee754, "Item123   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per123, gol_yjp123[31:0], dut_yjp123[31:0]);

if(col_index>=124) $fwrite(tri_report_ieee754, "Item124   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per124, gol_yjp124[63:32], dut_yjp124[63:32]);
if(col_index>=124) $fwrite(tri_report_ieee754, "Item124   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per124, gol_yjp124[31:0], dut_yjp124[31:0]);

if(col_index>=125) $fwrite(tri_report_ieee754, "Item125   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per125, gol_yjp125[63:32], dut_yjp125[63:32]);
if(col_index>=125) $fwrite(tri_report_ieee754, "Item125   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per125, gol_yjp125[31:0], dut_yjp125[31:0]);

if(col_index>=126) $fwrite(tri_report_ieee754, "Item126   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per126, gol_yjp126[63:32], dut_yjp126[63:32]);
if(col_index>=126) $fwrite(tri_report_ieee754, "Item126   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per126, gol_yjp126[31:0], dut_yjp126[31:0]);

if(col_index>=127) $fwrite(tri_report_ieee754, "Item127   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per127, gol_yjp127[63:32], dut_yjp127[63:32]);
if(col_index>=127) $fwrite(tri_report_ieee754, "Item127   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per127, gol_yjp127[31:0], dut_yjp127[31:0]);

if(col_index>=128) $fwrite(tri_report_ieee754, "Item128   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per128, gol_yjp128[63:32], dut_yjp128[63:32]);
if(col_index>=128) $fwrite(tri_report_ieee754, "Item128   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per128, gol_yjp128[31:0], dut_yjp128[31:0]);

if(col_index>=129) $fwrite(tri_report_ieee754, "Item129   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per129, gol_yjp129[63:32], dut_yjp129[63:32]);
if(col_index>=129) $fwrite(tri_report_ieee754, "Item129   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per129, gol_yjp129[31:0], dut_yjp129[31:0]);

if(col_index>=130) $fwrite(tri_report_ieee754, "Item130   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per130, gol_yjp130[63:32], dut_yjp130[63:32]);
if(col_index>=130) $fwrite(tri_report_ieee754, "Item130   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per130, gol_yjp130[31:0], dut_yjp130[31:0]);

if(col_index>=131) $fwrite(tri_report_ieee754, "Item131   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per131, gol_yjp131[63:32], dut_yjp131[63:32]);
if(col_index>=131) $fwrite(tri_report_ieee754, "Item131   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per131, gol_yjp131[31:0], dut_yjp131[31:0]);

if(col_index>=132) $fwrite(tri_report_ieee754, "Item132   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per132, gol_yjp132[63:32], dut_yjp132[63:32]);
if(col_index>=132) $fwrite(tri_report_ieee754, "Item132   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per132, gol_yjp132[31:0], dut_yjp132[31:0]);

if(col_index>=133) $fwrite(tri_report_ieee754, "Item133   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per133, gol_yjp133[63:32], dut_yjp133[63:32]);
if(col_index>=133) $fwrite(tri_report_ieee754, "Item133   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per133, gol_yjp133[31:0], dut_yjp133[31:0]);

if(col_index>=134) $fwrite(tri_report_ieee754, "Item134   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per134, gol_yjp134[63:32], dut_yjp134[63:32]);
if(col_index>=134) $fwrite(tri_report_ieee754, "Item134   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per134, gol_yjp134[31:0], dut_yjp134[31:0]);

if(col_index>=135) $fwrite(tri_report_ieee754, "Item135   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per135, gol_yjp135[63:32], dut_yjp135[63:32]);
if(col_index>=135) $fwrite(tri_report_ieee754, "Item135   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per135, gol_yjp135[31:0], dut_yjp135[31:0]);

if(col_index>=136) $fwrite(tri_report_ieee754, "Item136   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per136, gol_yjp136[63:32], dut_yjp136[63:32]);
if(col_index>=136) $fwrite(tri_report_ieee754, "Item136   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per136, gol_yjp136[31:0], dut_yjp136[31:0]);

if(col_index>=137) $fwrite(tri_report_ieee754, "Item137   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per137, gol_yjp137[63:32], dut_yjp137[63:32]);
if(col_index>=137) $fwrite(tri_report_ieee754, "Item137   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per137, gol_yjp137[31:0], dut_yjp137[31:0]);

if(col_index>=138) $fwrite(tri_report_ieee754, "Item138   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per138, gol_yjp138[63:32], dut_yjp138[63:32]);
if(col_index>=138) $fwrite(tri_report_ieee754, "Item138   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per138, gol_yjp138[31:0], dut_yjp138[31:0]);

if(col_index>=139) $fwrite(tri_report_ieee754, "Item139   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per139, gol_yjp139[63:32], dut_yjp139[63:32]);
if(col_index>=139) $fwrite(tri_report_ieee754, "Item139   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per139, gol_yjp139[31:0], dut_yjp139[31:0]);

if(col_index>=140) $fwrite(tri_report_ieee754, "Item140   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per140, gol_yjp140[63:32], dut_yjp140[63:32]);
if(col_index>=140) $fwrite(tri_report_ieee754, "Item140   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per140, gol_yjp140[31:0], dut_yjp140[31:0]);

if(col_index>=141) $fwrite(tri_report_ieee754, "Item141   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per141, gol_yjp141[63:32], dut_yjp141[63:32]);
if(col_index>=141) $fwrite(tri_report_ieee754, "Item141   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per141, gol_yjp141[31:0], dut_yjp141[31:0]);

if(col_index>=142) $fwrite(tri_report_ieee754, "Item142   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per142, gol_yjp142[63:32], dut_yjp142[63:32]);
if(col_index>=142) $fwrite(tri_report_ieee754, "Item142   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per142, gol_yjp142[31:0], dut_yjp142[31:0]);

if(col_index>=143) $fwrite(tri_report_ieee754, "Item143   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per143, gol_yjp143[63:32], dut_yjp143[63:32]);
if(col_index>=143) $fwrite(tri_report_ieee754, "Item143   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per143, gol_yjp143[31:0], dut_yjp143[31:0]);

if(col_index>=144) $fwrite(tri_report_ieee754, "Item144   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per144, gol_yjp144[63:32], dut_yjp144[63:32]);
if(col_index>=144) $fwrite(tri_report_ieee754, "Item144   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per144, gol_yjp144[31:0], dut_yjp144[31:0]);

if(col_index>=145) $fwrite(tri_report_ieee754, "Item145   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per145, gol_yjp145[63:32], dut_yjp145[63:32]);
if(col_index>=145) $fwrite(tri_report_ieee754, "Item145   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per145, gol_yjp145[31:0], dut_yjp145[31:0]);

if(col_index>=146) $fwrite(tri_report_ieee754, "Item146   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per146, gol_yjp146[63:32], dut_yjp146[63:32]);
if(col_index>=146) $fwrite(tri_report_ieee754, "Item146   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per146, gol_yjp146[31:0], dut_yjp146[31:0]);

if(col_index>=147) $fwrite(tri_report_ieee754, "Item147   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per147, gol_yjp147[63:32], dut_yjp147[63:32]);
if(col_index>=147) $fwrite(tri_report_ieee754, "Item147   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per147, gol_yjp147[31:0], dut_yjp147[31:0]);

if(col_index>=148) $fwrite(tri_report_ieee754, "Item148   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per148, gol_yjp148[63:32], dut_yjp148[63:32]);
if(col_index>=148) $fwrite(tri_report_ieee754, "Item148   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per148, gol_yjp148[31:0], dut_yjp148[31:0]);

if(col_index>=149) $fwrite(tri_report_ieee754, "Item149   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per149, gol_yjp149[63:32], dut_yjp149[63:32]);
if(col_index>=149) $fwrite(tri_report_ieee754, "Item149   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per149, gol_yjp149[31:0], dut_yjp149[31:0]);

if(col_index>=150) $fwrite(tri_report_ieee754, "Item150   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per150, gol_yjp150[63:32], dut_yjp150[63:32]);
if(col_index>=150) $fwrite(tri_report_ieee754, "Item150   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per150, gol_yjp150[31:0], dut_yjp150[31:0]);

if(col_index>=151) $fwrite(tri_report_ieee754, "Item151   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per151, gol_yjp151[63:32], dut_yjp151[63:32]);
if(col_index>=151) $fwrite(tri_report_ieee754, "Item151   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per151, gol_yjp151[31:0], dut_yjp151[31:0]);

if(col_index>=152) $fwrite(tri_report_ieee754, "Item152   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per152, gol_yjp152[63:32], dut_yjp152[63:32]);
if(col_index>=152) $fwrite(tri_report_ieee754, "Item152   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per152, gol_yjp152[31:0], dut_yjp152[31:0]);

if(col_index>=153) $fwrite(tri_report_ieee754, "Item153   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per153, gol_yjp153[63:32], dut_yjp153[63:32]);
if(col_index>=153) $fwrite(tri_report_ieee754, "Item153   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per153, gol_yjp153[31:0], dut_yjp153[31:0]);

if(col_index>=154) $fwrite(tri_report_ieee754, "Item154   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per154, gol_yjp154[63:32], dut_yjp154[63:32]);
if(col_index>=154) $fwrite(tri_report_ieee754, "Item154   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per154, gol_yjp154[31:0], dut_yjp154[31:0]);

if(col_index>=155) $fwrite(tri_report_ieee754, "Item155   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per155, gol_yjp155[63:32], dut_yjp155[63:32]);
if(col_index>=155) $fwrite(tri_report_ieee754, "Item155   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per155, gol_yjp155[31:0], dut_yjp155[31:0]);

if(col_index>=156) $fwrite(tri_report_ieee754, "Item156   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per156, gol_yjp156[63:32], dut_yjp156[63:32]);
if(col_index>=156) $fwrite(tri_report_ieee754, "Item156   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per156, gol_yjp156[31:0], dut_yjp156[31:0]);

if(col_index>=157) $fwrite(tri_report_ieee754, "Item157   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per157, gol_yjp157[63:32], dut_yjp157[63:32]);
if(col_index>=157) $fwrite(tri_report_ieee754, "Item157   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per157, gol_yjp157[31:0], dut_yjp157[31:0]);

if(col_index>=158) $fwrite(tri_report_ieee754, "Item158   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per158, gol_yjp158[63:32], dut_yjp158[63:32]);
if(col_index>=158) $fwrite(tri_report_ieee754, "Item158   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per158, gol_yjp158[31:0], dut_yjp158[31:0]);

if(col_index>=159) $fwrite(tri_report_ieee754, "Item159   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per159, gol_yjp159[63:32], dut_yjp159[63:32]);
if(col_index>=159) $fwrite(tri_report_ieee754, "Item159   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per159, gol_yjp159[31:0], dut_yjp159[31:0]);

if(col_index>=160) $fwrite(tri_report_ieee754, "Item160   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per160, gol_yjp160[63:32], dut_yjp160[63:32]);
if(col_index>=160) $fwrite(tri_report_ieee754, "Item160   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per160, gol_yjp160[31:0], dut_yjp160[31:0]);

if(col_index>=161) $fwrite(tri_report_ieee754, "Item161   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per161, gol_yjp161[63:32], dut_yjp161[63:32]);
if(col_index>=161) $fwrite(tri_report_ieee754, "Item161   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per161, gol_yjp161[31:0], dut_yjp161[31:0]);

if(col_index>=162) $fwrite(tri_report_ieee754, "Item162   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per162, gol_yjp162[63:32], dut_yjp162[63:32]);
if(col_index>=162) $fwrite(tri_report_ieee754, "Item162   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per162, gol_yjp162[31:0], dut_yjp162[31:0]);

if(col_index>=163) $fwrite(tri_report_ieee754, "Item163   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per163, gol_yjp163[63:32], dut_yjp163[63:32]);
if(col_index>=163) $fwrite(tri_report_ieee754, "Item163   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per163, gol_yjp163[31:0], dut_yjp163[31:0]);

if(col_index>=164) $fwrite(tri_report_ieee754, "Item164   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per164, gol_yjp164[63:32], dut_yjp164[63:32]);
if(col_index>=164) $fwrite(tri_report_ieee754, "Item164   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per164, gol_yjp164[31:0], dut_yjp164[31:0]);

if(col_index>=165) $fwrite(tri_report_ieee754, "Item165   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per165, gol_yjp165[63:32], dut_yjp165[63:32]);
if(col_index>=165) $fwrite(tri_report_ieee754, "Item165   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per165, gol_yjp165[31:0], dut_yjp165[31:0]);

if(col_index>=166) $fwrite(tri_report_ieee754, "Item166   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per166, gol_yjp166[63:32], dut_yjp166[63:32]);
if(col_index>=166) $fwrite(tri_report_ieee754, "Item166   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per166, gol_yjp166[31:0], dut_yjp166[31:0]);

if(col_index>=167) $fwrite(tri_report_ieee754, "Item167   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per167, gol_yjp167[63:32], dut_yjp167[63:32]);
if(col_index>=167) $fwrite(tri_report_ieee754, "Item167   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per167, gol_yjp167[31:0], dut_yjp167[31:0]);

if(col_index>=168) $fwrite(tri_report_ieee754, "Item168   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per168, gol_yjp168[63:32], dut_yjp168[63:32]);
if(col_index>=168) $fwrite(tri_report_ieee754, "Item168   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per168, gol_yjp168[31:0], dut_yjp168[31:0]);

if(col_index>=169) $fwrite(tri_report_ieee754, "Item169   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per169, gol_yjp169[63:32], dut_yjp169[63:32]);
if(col_index>=169) $fwrite(tri_report_ieee754, "Item169   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per169, gol_yjp169[31:0], dut_yjp169[31:0]);

if(col_index>=170) $fwrite(tri_report_ieee754, "Item170   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per170, gol_yjp170[63:32], dut_yjp170[63:32]);
if(col_index>=170) $fwrite(tri_report_ieee754, "Item170   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per170, gol_yjp170[31:0], dut_yjp170[31:0]);

if(col_index>=171) $fwrite(tri_report_ieee754, "Item171   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per171, gol_yjp171[63:32], dut_yjp171[63:32]);
if(col_index>=171) $fwrite(tri_report_ieee754, "Item171   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per171, gol_yjp171[31:0], dut_yjp171[31:0]);

if(col_index>=172) $fwrite(tri_report_ieee754, "Item172   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per172, gol_yjp172[63:32], dut_yjp172[63:32]);
if(col_index>=172) $fwrite(tri_report_ieee754, "Item172   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per172, gol_yjp172[31:0], dut_yjp172[31:0]);

if(col_index>=173) $fwrite(tri_report_ieee754, "Item173   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per173, gol_yjp173[63:32], dut_yjp173[63:32]);
if(col_index>=173) $fwrite(tri_report_ieee754, "Item173   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per173, gol_yjp173[31:0], dut_yjp173[31:0]);

if(col_index>=174) $fwrite(tri_report_ieee754, "Item174   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per174, gol_yjp174[63:32], dut_yjp174[63:32]);
if(col_index>=174) $fwrite(tri_report_ieee754, "Item174   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per174, gol_yjp174[31:0], dut_yjp174[31:0]);

if(col_index>=175) $fwrite(tri_report_ieee754, "Item175   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per175, gol_yjp175[63:32], dut_yjp175[63:32]);
if(col_index>=175) $fwrite(tri_report_ieee754, "Item175   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per175, gol_yjp175[31:0], dut_yjp175[31:0]);

if(col_index>=176) $fwrite(tri_report_ieee754, "Item176   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per176, gol_yjp176[63:32], dut_yjp176[63:32]);
if(col_index>=176) $fwrite(tri_report_ieee754, "Item176   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per176, gol_yjp176[31:0], dut_yjp176[31:0]);

if(col_index>=177) $fwrite(tri_report_ieee754, "Item177   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per177, gol_yjp177[63:32], dut_yjp177[63:32]);
if(col_index>=177) $fwrite(tri_report_ieee754, "Item177   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per177, gol_yjp177[31:0], dut_yjp177[31:0]);

if(col_index>=178) $fwrite(tri_report_ieee754, "Item178   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per178, gol_yjp178[63:32], dut_yjp178[63:32]);
if(col_index>=178) $fwrite(tri_report_ieee754, "Item178   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per178, gol_yjp178[31:0], dut_yjp178[31:0]);

if(col_index>=179) $fwrite(tri_report_ieee754, "Item179   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per179, gol_yjp179[63:32], dut_yjp179[63:32]);
if(col_index>=179) $fwrite(tri_report_ieee754, "Item179   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per179, gol_yjp179[31:0], dut_yjp179[31:0]);

if(col_index>=180) $fwrite(tri_report_ieee754, "Item180   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per180, gol_yjp180[63:32], dut_yjp180[63:32]);
if(col_index>=180) $fwrite(tri_report_ieee754, "Item180   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per180, gol_yjp180[31:0], dut_yjp180[31:0]);

if(col_index>=181) $fwrite(tri_report_ieee754, "Item181   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per181, gol_yjp181[63:32], dut_yjp181[63:32]);
if(col_index>=181) $fwrite(tri_report_ieee754, "Item181   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per181, gol_yjp181[31:0], dut_yjp181[31:0]);

if(col_index>=182) $fwrite(tri_report_ieee754, "Item182   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per182, gol_yjp182[63:32], dut_yjp182[63:32]);
if(col_index>=182) $fwrite(tri_report_ieee754, "Item182   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per182, gol_yjp182[31:0], dut_yjp182[31:0]);

if(col_index>=183) $fwrite(tri_report_ieee754, "Item183   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per183, gol_yjp183[63:32], dut_yjp183[63:32]);
if(col_index>=183) $fwrite(tri_report_ieee754, "Item183   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per183, gol_yjp183[31:0], dut_yjp183[31:0]);

if(col_index>=184) $fwrite(tri_report_ieee754, "Item184   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per184, gol_yjp184[63:32], dut_yjp184[63:32]);
if(col_index>=184) $fwrite(tri_report_ieee754, "Item184   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per184, gol_yjp184[31:0], dut_yjp184[31:0]);

if(col_index>=185) $fwrite(tri_report_ieee754, "Item185   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per185, gol_yjp185[63:32], dut_yjp185[63:32]);
if(col_index>=185) $fwrite(tri_report_ieee754, "Item185   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per185, gol_yjp185[31:0], dut_yjp185[31:0]);

if(col_index>=186) $fwrite(tri_report_ieee754, "Item186   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per186, gol_yjp186[63:32], dut_yjp186[63:32]);
if(col_index>=186) $fwrite(tri_report_ieee754, "Item186   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per186, gol_yjp186[31:0], dut_yjp186[31:0]);

if(col_index>=187) $fwrite(tri_report_ieee754, "Item187   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per187, gol_yjp187[63:32], dut_yjp187[63:32]);
if(col_index>=187) $fwrite(tri_report_ieee754, "Item187   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per187, gol_yjp187[31:0], dut_yjp187[31:0]);

if(col_index>=188) $fwrite(tri_report_ieee754, "Item188   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per188, gol_yjp188[63:32], dut_yjp188[63:32]);
if(col_index>=188) $fwrite(tri_report_ieee754, "Item188   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per188, gol_yjp188[31:0], dut_yjp188[31:0]);

if(col_index>=189) $fwrite(tri_report_ieee754, "Item189   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per189, gol_yjp189[63:32], dut_yjp189[63:32]);
if(col_index>=189) $fwrite(tri_report_ieee754, "Item189   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per189, gol_yjp189[31:0], dut_yjp189[31:0]);

if(col_index>=190) $fwrite(tri_report_ieee754, "Item190   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per190, gol_yjp190[63:32], dut_yjp190[63:32]);
if(col_index>=190) $fwrite(tri_report_ieee754, "Item190   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per190, gol_yjp190[31:0], dut_yjp190[31:0]);

if(col_index>=191) $fwrite(tri_report_ieee754, "Item191   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per191, gol_yjp191[63:32], dut_yjp191[63:32]);
if(col_index>=191) $fwrite(tri_report_ieee754, "Item191   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per191, gol_yjp191[31:0], dut_yjp191[31:0]);

if(col_index>=192) $fwrite(tri_report_ieee754, "Item192   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per192, gol_yjp192[63:32], dut_yjp192[63:32]);
if(col_index>=192) $fwrite(tri_report_ieee754, "Item192   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per192, gol_yjp192[31:0], dut_yjp192[31:0]);

if(col_index>=193) $fwrite(tri_report_ieee754, "Item193   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per193, gol_yjp193[63:32], dut_yjp193[63:32]);
if(col_index>=193) $fwrite(tri_report_ieee754, "Item193   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per193, gol_yjp193[31:0], dut_yjp193[31:0]);

if(col_index>=194) $fwrite(tri_report_ieee754, "Item194   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per194, gol_yjp194[63:32], dut_yjp194[63:32]);
if(col_index>=194) $fwrite(tri_report_ieee754, "Item194   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per194, gol_yjp194[31:0], dut_yjp194[31:0]);

if(col_index>=195) $fwrite(tri_report_ieee754, "Item195   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per195, gol_yjp195[63:32], dut_yjp195[63:32]);
if(col_index>=195) $fwrite(tri_report_ieee754, "Item195   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per195, gol_yjp195[31:0], dut_yjp195[31:0]);

if(col_index>=196) $fwrite(tri_report_ieee754, "Item196   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per196, gol_yjp196[63:32], dut_yjp196[63:32]);
if(col_index>=196) $fwrite(tri_report_ieee754, "Item196   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per196, gol_yjp196[31:0], dut_yjp196[31:0]);

if(col_index>=197) $fwrite(tri_report_ieee754, "Item197   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per197, gol_yjp197[63:32], dut_yjp197[63:32]);
if(col_index>=197) $fwrite(tri_report_ieee754, "Item197   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per197, gol_yjp197[31:0], dut_yjp197[31:0]);

if(col_index>=198) $fwrite(tri_report_ieee754, "Item198   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per198, gol_yjp198[63:32], dut_yjp198[63:32]);
if(col_index>=198) $fwrite(tri_report_ieee754, "Item198   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per198, gol_yjp198[31:0], dut_yjp198[31:0]);

if(col_index>=199) $fwrite(tri_report_ieee754, "Item199   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per199, gol_yjp199[63:32], dut_yjp199[63:32]);
if(col_index>=199) $fwrite(tri_report_ieee754, "Item199   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per199, gol_yjp199[31:0], dut_yjp199[31:0]);

if(col_index>=200) $fwrite(tri_report_ieee754, "Item200   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per200, gol_yjp200[63:32], dut_yjp200[63:32]);
if(col_index>=200) $fwrite(tri_report_ieee754, "Item200   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per200, gol_yjp200[31:0], dut_yjp200[31:0]);

if(col_index>=201) $fwrite(tri_report_ieee754, "Item201   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per201, gol_yjp201[63:32], dut_yjp201[63:32]);
if(col_index>=201) $fwrite(tri_report_ieee754, "Item201   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per201, gol_yjp201[31:0], dut_yjp201[31:0]);

if(col_index>=202) $fwrite(tri_report_ieee754, "Item202   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per202, gol_yjp202[63:32], dut_yjp202[63:32]);
if(col_index>=202) $fwrite(tri_report_ieee754, "Item202   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per202, gol_yjp202[31:0], dut_yjp202[31:0]);

if(col_index>=203) $fwrite(tri_report_ieee754, "Item203   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per203, gol_yjp203[63:32], dut_yjp203[63:32]);
if(col_index>=203) $fwrite(tri_report_ieee754, "Item203   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per203, gol_yjp203[31:0], dut_yjp203[31:0]);

if(col_index>=204) $fwrite(tri_report_ieee754, "Item204   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per204, gol_yjp204[63:32], dut_yjp204[63:32]);
if(col_index>=204) $fwrite(tri_report_ieee754, "Item204   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per204, gol_yjp204[31:0], dut_yjp204[31:0]);

if(col_index>=205) $fwrite(tri_report_ieee754, "Item205   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per205, gol_yjp205[63:32], dut_yjp205[63:32]);
if(col_index>=205) $fwrite(tri_report_ieee754, "Item205   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per205, gol_yjp205[31:0], dut_yjp205[31:0]);

if(col_index>=206) $fwrite(tri_report_ieee754, "Item206   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per206, gol_yjp206[63:32], dut_yjp206[63:32]);
if(col_index>=206) $fwrite(tri_report_ieee754, "Item206   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per206, gol_yjp206[31:0], dut_yjp206[31:0]);

if(col_index>=207) $fwrite(tri_report_ieee754, "Item207   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per207, gol_yjp207[63:32], dut_yjp207[63:32]);
if(col_index>=207) $fwrite(tri_report_ieee754, "Item207   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per207, gol_yjp207[31:0], dut_yjp207[31:0]);

if(col_index>=208) $fwrite(tri_report_ieee754, "Item208   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per208, gol_yjp208[63:32], dut_yjp208[63:32]);
if(col_index>=208) $fwrite(tri_report_ieee754, "Item208   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per208, gol_yjp208[31:0], dut_yjp208[31:0]);

if(col_index>=209) $fwrite(tri_report_ieee754, "Item209   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per209, gol_yjp209[63:32], dut_yjp209[63:32]);
if(col_index>=209) $fwrite(tri_report_ieee754, "Item209   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per209, gol_yjp209[31:0], dut_yjp209[31:0]);

if(col_index>=210) $fwrite(tri_report_ieee754, "Item210   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per210, gol_yjp210[63:32], dut_yjp210[63:32]);
if(col_index>=210) $fwrite(tri_report_ieee754, "Item210   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per210, gol_yjp210[31:0], dut_yjp210[31:0]);

if(col_index>=211) $fwrite(tri_report_ieee754, "Item211   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per211, gol_yjp211[63:32], dut_yjp211[63:32]);
if(col_index>=211) $fwrite(tri_report_ieee754, "Item211   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per211, gol_yjp211[31:0], dut_yjp211[31:0]);

if(col_index>=212) $fwrite(tri_report_ieee754, "Item212   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per212, gol_yjp212[63:32], dut_yjp212[63:32]);
if(col_index>=212) $fwrite(tri_report_ieee754, "Item212   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per212, gol_yjp212[31:0], dut_yjp212[31:0]);

if(col_index>=213) $fwrite(tri_report_ieee754, "Item213   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per213, gol_yjp213[63:32], dut_yjp213[63:32]);
if(col_index>=213) $fwrite(tri_report_ieee754, "Item213   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per213, gol_yjp213[31:0], dut_yjp213[31:0]);

if(col_index>=214) $fwrite(tri_report_ieee754, "Item214   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per214, gol_yjp214[63:32], dut_yjp214[63:32]);
if(col_index>=214) $fwrite(tri_report_ieee754, "Item214   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per214, gol_yjp214[31:0], dut_yjp214[31:0]);

if(col_index>=215) $fwrite(tri_report_ieee754, "Item215   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per215, gol_yjp215[63:32], dut_yjp215[63:32]);
if(col_index>=215) $fwrite(tri_report_ieee754, "Item215   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per215, gol_yjp215[31:0], dut_yjp215[31:0]);

if(col_index>=216) $fwrite(tri_report_ieee754, "Item216   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per216, gol_yjp216[63:32], dut_yjp216[63:32]);
if(col_index>=216) $fwrite(tri_report_ieee754, "Item216   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per216, gol_yjp216[31:0], dut_yjp216[31:0]);

if(col_index>=217) $fwrite(tri_report_ieee754, "Item217   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per217, gol_yjp217[63:32], dut_yjp217[63:32]);
if(col_index>=217) $fwrite(tri_report_ieee754, "Item217   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per217, gol_yjp217[31:0], dut_yjp217[31:0]);

if(col_index>=218) $fwrite(tri_report_ieee754, "Item218   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per218, gol_yjp218[63:32], dut_yjp218[63:32]);
if(col_index>=218) $fwrite(tri_report_ieee754, "Item218   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per218, gol_yjp218[31:0], dut_yjp218[31:0]);

if(col_index>=219) $fwrite(tri_report_ieee754, "Item219   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per219, gol_yjp219[63:32], dut_yjp219[63:32]);
if(col_index>=219) $fwrite(tri_report_ieee754, "Item219   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per219, gol_yjp219[31:0], dut_yjp219[31:0]);

if(col_index>=220) $fwrite(tri_report_ieee754, "Item220   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per220, gol_yjp220[63:32], dut_yjp220[63:32]);
if(col_index>=220) $fwrite(tri_report_ieee754, "Item220   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per220, gol_yjp220[31:0], dut_yjp220[31:0]);

if(col_index>=221) $fwrite(tri_report_ieee754, "Item221   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per221, gol_yjp221[63:32], dut_yjp221[63:32]);
if(col_index>=221) $fwrite(tri_report_ieee754, "Item221   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per221, gol_yjp221[31:0], dut_yjp221[31:0]);

if(col_index>=222) $fwrite(tri_report_ieee754, "Item222   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per222, gol_yjp222[63:32], dut_yjp222[63:32]);
if(col_index>=222) $fwrite(tri_report_ieee754, "Item222   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per222, gol_yjp222[31:0], dut_yjp222[31:0]);

if(col_index>=223) $fwrite(tri_report_ieee754, "Item223   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per223, gol_yjp223[63:32], dut_yjp223[63:32]);
if(col_index>=223) $fwrite(tri_report_ieee754, "Item223   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per223, gol_yjp223[31:0], dut_yjp223[31:0]);

if(col_index>=224) $fwrite(tri_report_ieee754, "Item224   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per224, gol_yjp224[63:32], dut_yjp224[63:32]);
if(col_index>=224) $fwrite(tri_report_ieee754, "Item224   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per224, gol_yjp224[31:0], dut_yjp224[31:0]);

if(col_index>=225) $fwrite(tri_report_ieee754, "Item225   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per225, gol_yjp225[63:32], dut_yjp225[63:32]);
if(col_index>=225) $fwrite(tri_report_ieee754, "Item225   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per225, gol_yjp225[31:0], dut_yjp225[31:0]);

if(col_index>=226) $fwrite(tri_report_ieee754, "Item226   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per226, gol_yjp226[63:32], dut_yjp226[63:32]);
if(col_index>=226) $fwrite(tri_report_ieee754, "Item226   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per226, gol_yjp226[31:0], dut_yjp226[31:0]);

if(col_index>=227) $fwrite(tri_report_ieee754, "Item227   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per227, gol_yjp227[63:32], dut_yjp227[63:32]);
if(col_index>=227) $fwrite(tri_report_ieee754, "Item227   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per227, gol_yjp227[31:0], dut_yjp227[31:0]);

if(col_index>=228) $fwrite(tri_report_ieee754, "Item228   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per228, gol_yjp228[63:32], dut_yjp228[63:32]);
if(col_index>=228) $fwrite(tri_report_ieee754, "Item228   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per228, gol_yjp228[31:0], dut_yjp228[31:0]);

if(col_index>=229) $fwrite(tri_report_ieee754, "Item229   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per229, gol_yjp229[63:32], dut_yjp229[63:32]);
if(col_index>=229) $fwrite(tri_report_ieee754, "Item229   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per229, gol_yjp229[31:0], dut_yjp229[31:0]);

if(col_index>=230) $fwrite(tri_report_ieee754, "Item230   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per230, gol_yjp230[63:32], dut_yjp230[63:32]);
if(col_index>=230) $fwrite(tri_report_ieee754, "Item230   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per230, gol_yjp230[31:0], dut_yjp230[31:0]);

if(col_index>=231) $fwrite(tri_report_ieee754, "Item231   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per231, gol_yjp231[63:32], dut_yjp231[63:32]);
if(col_index>=231) $fwrite(tri_report_ieee754, "Item231   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per231, gol_yjp231[31:0], dut_yjp231[31:0]);

if(col_index>=232) $fwrite(tri_report_ieee754, "Item232   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per232, gol_yjp232[63:32], dut_yjp232[63:32]);
if(col_index>=232) $fwrite(tri_report_ieee754, "Item232   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per232, gol_yjp232[31:0], dut_yjp232[31:0]);

if(col_index>=233) $fwrite(tri_report_ieee754, "Item233   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per233, gol_yjp233[63:32], dut_yjp233[63:32]);
if(col_index>=233) $fwrite(tri_report_ieee754, "Item233   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per233, gol_yjp233[31:0], dut_yjp233[31:0]);

if(col_index>=234) $fwrite(tri_report_ieee754, "Item234   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per234, gol_yjp234[63:32], dut_yjp234[63:32]);
if(col_index>=234) $fwrite(tri_report_ieee754, "Item234   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per234, gol_yjp234[31:0], dut_yjp234[31:0]);

if(col_index>=235) $fwrite(tri_report_ieee754, "Item235   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per235, gol_yjp235[63:32], dut_yjp235[63:32]);
if(col_index>=235) $fwrite(tri_report_ieee754, "Item235   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per235, gol_yjp235[31:0], dut_yjp235[31:0]);

if(col_index>=236) $fwrite(tri_report_ieee754, "Item236   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per236, gol_yjp236[63:32], dut_yjp236[63:32]);
if(col_index>=236) $fwrite(tri_report_ieee754, "Item236   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per236, gol_yjp236[31:0], dut_yjp236[31:0]);

if(col_index>=237) $fwrite(tri_report_ieee754, "Item237   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per237, gol_yjp237[63:32], dut_yjp237[63:32]);
if(col_index>=237) $fwrite(tri_report_ieee754, "Item237   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per237, gol_yjp237[31:0], dut_yjp237[31:0]);

if(col_index>=238) $fwrite(tri_report_ieee754, "Item238   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per238, gol_yjp238[63:32], dut_yjp238[63:32]);
if(col_index>=238) $fwrite(tri_report_ieee754, "Item238   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per238, gol_yjp238[31:0], dut_yjp238[31:0]);

if(col_index>=239) $fwrite(tri_report_ieee754, "Item239   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per239, gol_yjp239[63:32], dut_yjp239[63:32]);
if(col_index>=239) $fwrite(tri_report_ieee754, "Item239   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per239, gol_yjp239[31:0], dut_yjp239[31:0]);

if(col_index>=240) $fwrite(tri_report_ieee754, "Item240   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per240, gol_yjp240[63:32], dut_yjp240[63:32]);
if(col_index>=240) $fwrite(tri_report_ieee754, "Item240   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per240, gol_yjp240[31:0], dut_yjp240[31:0]);

if(col_index>=241) $fwrite(tri_report_ieee754, "Item241   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per241, gol_yjp241[63:32], dut_yjp241[63:32]);
if(col_index>=241) $fwrite(tri_report_ieee754, "Item241   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per241, gol_yjp241[31:0], dut_yjp241[31:0]);

if(col_index>=242) $fwrite(tri_report_ieee754, "Item242   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per242, gol_yjp242[63:32], dut_yjp242[63:32]);
if(col_index>=242) $fwrite(tri_report_ieee754, "Item242   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per242, gol_yjp242[31:0], dut_yjp242[31:0]);

if(col_index>=243) $fwrite(tri_report_ieee754, "Item243   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per243, gol_yjp243[63:32], dut_yjp243[63:32]);
if(col_index>=243) $fwrite(tri_report_ieee754, "Item243   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per243, gol_yjp243[31:0], dut_yjp243[31:0]);

if(col_index>=244) $fwrite(tri_report_ieee754, "Item244   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per244, gol_yjp244[63:32], dut_yjp244[63:32]);
if(col_index>=244) $fwrite(tri_report_ieee754, "Item244   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per244, gol_yjp244[31:0], dut_yjp244[31:0]);

if(col_index>=245) $fwrite(tri_report_ieee754, "Item245   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per245, gol_yjp245[63:32], dut_yjp245[63:32]);
if(col_index>=245) $fwrite(tri_report_ieee754, "Item245   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per245, gol_yjp245[31:0], dut_yjp245[31:0]);

if(col_index>=246) $fwrite(tri_report_ieee754, "Item246   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per246, gol_yjp246[63:32], dut_yjp246[63:32]);
if(col_index>=246) $fwrite(tri_report_ieee754, "Item246   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per246, gol_yjp246[31:0], dut_yjp246[31:0]);

if(col_index>=247) $fwrite(tri_report_ieee754, "Item247   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per247, gol_yjp247[63:32], dut_yjp247[63:32]);
if(col_index>=247) $fwrite(tri_report_ieee754, "Item247   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per247, gol_yjp247[31:0], dut_yjp247[31:0]);

if(col_index>=248) $fwrite(tri_report_ieee754, "Item248   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per248, gol_yjp248[63:32], dut_yjp248[63:32]);
if(col_index>=248) $fwrite(tri_report_ieee754, "Item248   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per248, gol_yjp248[31:0], dut_yjp248[31:0]);

if(col_index>=249) $fwrite(tri_report_ieee754, "Item249   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per249, gol_yjp249[63:32], dut_yjp249[63:32]);
if(col_index>=249) $fwrite(tri_report_ieee754, "Item249   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per249, gol_yjp249[31:0], dut_yjp249[31:0]);

if(col_index>=250) $fwrite(tri_report_ieee754, "Item250   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per250, gol_yjp250[63:32], dut_yjp250[63:32]);
if(col_index>=250) $fwrite(tri_report_ieee754, "Item250   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per250, gol_yjp250[31:0], dut_yjp250[31:0]);

if(col_index>=251) $fwrite(tri_report_ieee754, "Item251   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per251, gol_yjp251[63:32], dut_yjp251[63:32]);
if(col_index>=251) $fwrite(tri_report_ieee754, "Item251   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per251, gol_yjp251[31:0], dut_yjp251[31:0]);

if(col_index>=252) $fwrite(tri_report_ieee754, "Item252   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per252, gol_yjp252[63:32], dut_yjp252[63:32]);
if(col_index>=252) $fwrite(tri_report_ieee754, "Item252   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per252, gol_yjp252[31:0], dut_yjp252[31:0]);

if(col_index>=253) $fwrite(tri_report_ieee754, "Item253   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per253, gol_yjp253[63:32], dut_yjp253[63:32]);
if(col_index>=253) $fwrite(tri_report_ieee754, "Item253   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per253, gol_yjp253[31:0], dut_yjp253[31:0]);

if(col_index>=254) $fwrite(tri_report_ieee754, "Item254   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per254, gol_yjp254[63:32], dut_yjp254[63:32]);
if(col_index>=254) $fwrite(tri_report_ieee754, "Item254   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per254, gol_yjp254[31:0], dut_yjp254[31:0]);

if(col_index>=255) $fwrite(tri_report_ieee754, "Item255   error re percent: %f%%, golden re result: %h, dut re result: %h\n", err_re_per255, gol_yjp255[63:32], dut_yjp255[63:32]);
if(col_index>=255) $fwrite(tri_report_ieee754, "Item255   error im percent: %f%%, golden im result: %h, dut im result: %h\n", err_im_per255, gol_yjp255[31:0], dut_yjp255[31:0]);

`endif

// ST_WIDTH_INF_16
