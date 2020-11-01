<?php


namespace vektah\parser_combinator\test\language\css;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\language\css\CssParser;

class LargeCssFileTest extends TestCase
{
    /** @var CssParser */
    private $parser;

    public function setUp() {
        $this->parser = new CssParser();
    }

    public function testLargeCssFile()
    {
        $this->parser->parseString($this->css());
    }

    private function css() {
        return <<< HERE
/* ---------- Color Pallet ---------- */
/* ---------- Global Variables ---------- */
/**********************************************
* Ink v1.0.3 - Copyright 2013 ZURB Inc        *
**********************************************/
/* Client-specific Styles & Reset */
/* line 7, ../../../../scss/email/_default.scss */
#outlook a {
  padding: 0;
}

/* line 11, ../../../../scss/email/_default.scss */
body {
  width: 100% !important;
  -webkit-text-size-adjust: 100%;
  -ms-text-size-adjust: 100%;
  margin: 0;
  padding: 0;
}

/* line 19, ../../../../scss/email/_default.scss */
.ExternalClass {
  width: 100%;
}

/* line 28, ../../../../scss/email/_default.scss */
.ExternalClass,
.ExternalClass p,
.ExternalClass span,
.ExternalClass font,
.ExternalClass td,
.ExternalClass div {
  line-height: 100%;
}

/* line 32, ../../../../scss/email/_default.scss */
#backgroundTable {
  margin: 0;
  padding: 0;
  width: 100% !important;
  line-height: 100% !important;
}

/* line 39, ../../../../scss/email/_default.scss */
img {
  outline: none;
  text-decoration: none;
  -ms-interpolation-mode: bicubic;
  width: auto;
  max-width: 100%;
  clear: both;
  display: block;
}

/* line 49, ../../../../scss/email/_default.scss */
center {
  width: 100%;
  min-width: 580px;
}

/* line 54, ../../../../scss/email/_default.scss */
a img {
  border: none;
}

/* line 58, ../../../../scss/email/_default.scss */
p {
  margin: 0 0 0 10px;
}

/* line 62, ../../../../scss/email/_default.scss */
table {
  border-spacing: 0;
  border-collapse: collapse;
}

/* line 67, ../../../../scss/email/_default.scss */
td {
  word-break: break-word;
  -webkit-hyphens: auto;
  -moz-hyphens: auto;
  hyphens: auto;
  border-collapse: collapse !important;
}

/* line 75, ../../../../scss/email/_default.scss */
table, tr, td {
  padding: 0;
  vertical-align: top;
  text-align: left;
}

/* line 81, ../../../../scss/email/_default.scss */
hr {
  color: #d9d9d9;
  background-color: #d9d9d9;
  height: 1px;
  border: none;
}

/* Responsive Grid */
/* line 92, ../../../../scss/email/_default.scss */
table.body,
table.footer,
table.unsubscribe {
  height: 100%;
  width: 100%;
}

/* line 97, ../../../../scss/email/_default.scss */
table.container {
  width: 580px;
  margin: 0 auto;
  text-align: inherit;
}

/* line 103, ../../../../scss/email/_default.scss */
table.row {
  padding: 0px;
  width: 100%;
  position: relative;
}

/* line 109, ../../../../scss/email/_default.scss */
table.container table.row {
  display: block;
}

/* line 113, ../../../../scss/email/_default.scss */
td.wrapper {
  padding: 10px 20px 0px 0px;
  position: relative;
}

/* line 119, ../../../../scss/email/_default.scss */
table.columns,
table.column {
  margin: 0 auto;
}

/* line 124, ../../../../scss/email/_default.scss */
table.columns td,
table.column td {
  padding: 0px 0px 10px;
}

/* line 131, ../../../../scss/email/_default.scss */
table.columns td.sub-columns,
table.column td.sub-columns,
table.columns td.sub-column,
table.column td.sub-column {
  padding-right: 3.448276%;
}

/* line 136, ../../../../scss/email/_default.scss */
table.row td.last,
table.container td.last {
  padding-right: 0px;
}

/* line 140, ../../../../scss/email/_default.scss */
table.one {
  width: 30px;
}

/* line 141, ../../../../scss/email/_default.scss */
table.two {
  width: 80px;
}

/* line 142, ../../../../scss/email/_default.scss */
table.three {
  width: 130px;
}

/* line 143, ../../../../scss/email/_default.scss */
table.four {
  width: 180px;
}

/* line 144, ../../../../scss/email/_default.scss */
table.five {
  width: 230px;
}

/* line 145, ../../../../scss/email/_default.scss */
table.six {
  width: 280px;
}

/* line 146, ../../../../scss/email/_default.scss */
table.seven {
  width: 330px;
}

/* line 147, ../../../../scss/email/_default.scss */
table.eight {
  width: 380px;
}

/* line 148, ../../../../scss/email/_default.scss */
table.nine {
  width: 430px;
}

/* line 149, ../../../../scss/email/_default.scss */
table.ten {
  width: 480px;
}

/* line 150, ../../../../scss/email/_default.scss */
table.eleven {
  width: 530px;
}

/* line 151, ../../../../scss/email/_default.scss */
table.twelve {
  width: 580px;
}

/* line 153, ../../../../scss/email/_default.scss */
table.one center {
  min-width: 30px;
}

/* line 154, ../../../../scss/email/_default.scss */
table.two center {
  min-width: 80px;
}

/* line 155, ../../../../scss/email/_default.scss */
table.three center {
  min-width: 130px;
}

/* line 156, ../../../../scss/email/_default.scss */
table.four center {
  min-width: 180px;
}

/* line 157, ../../../../scss/email/_default.scss */
table.five center {
  min-width: 230px;
}

/* line 158, ../../../../scss/email/_default.scss */
table.six center {
  min-width: 280px;
}

/* line 159, ../../../../scss/email/_default.scss */
table.seven center {
  min-width: 330px;
}

/* line 160, ../../../../scss/email/_default.scss */
table.eight center {
  min-width: 380px;
}

/* line 161, ../../../../scss/email/_default.scss */
table.nine center {
  min-width: 430px;
}

/* line 162, ../../../../scss/email/_default.scss */
table.ten center {
  min-width: 480px;
}

/* line 163, ../../../../scss/email/_default.scss */
table.eleven center {
  min-width: 530px;
}

/* line 164, ../../../../scss/email/_default.scss */
table.twelve center {
  min-width: 580px;
}

/* line 166, ../../../../scss/email/_default.scss */
td.one {
  width: 8.333333% !important;
}

/* line 167, ../../../../scss/email/_default.scss */
td.two {
  width: 16.666666% !important;
}

/* line 168, ../../../../scss/email/_default.scss */
td.three {
  width: 25% !important;
}

/* line 169, ../../../../scss/email/_default.scss */
td.four {
  width: 33.333333% !important;
}

/* line 170, ../../../../scss/email/_default.scss */
td.five {
  width: 41.666666% !important;
}

/* line 171, ../../../../scss/email/_default.scss */
td.six {
  width: 50% !important;
}

/* line 172, ../../../../scss/email/_default.scss */
td.seven {
  width: 58.333333% !important;
}

/* line 173, ../../../../scss/email/_default.scss */
td.eight {
  width: 66.666666% !important;
}

/* line 174, ../../../../scss/email/_default.scss */
td.nine {
  width: 75% !important;
}

/* line 175, ../../../../scss/email/_default.scss */
td.ten {
  width: 83.333333% !important;
}

/* line 176, ../../../../scss/email/_default.scss */
td.eleven {
  width: 91.666666% !important;
}

/* line 177, ../../../../scss/email/_default.scss */
td.twelve {
  width: 100% !important;
}

/* line 179, ../../../../scss/email/_default.scss */
td.offset-by-one {
  padding-left: 50px;
}

/* line 180, ../../../../scss/email/_default.scss */
td.offset-by-two {
  padding-left: 100px;
}

/* line 181, ../../../../scss/email/_default.scss */
td.offset-by-three {
  padding-left: 150px;
}

/* line 182, ../../../../scss/email/_default.scss */
td.offset-by-four {
  padding-left: 200px;
}

/* line 183, ../../../../scss/email/_default.scss */
td.offset-by-five {
  padding-left: 250px;
}

/* line 184, ../../../../scss/email/_default.scss */
td.offset-by-six {
  padding-left: 300px;
}

/* line 185, ../../../../scss/email/_default.scss */
td.offset-by-seven {
  padding-left: 350px;
}

/* line 186, ../../../../scss/email/_default.scss */
td.offset-by-eight {
  padding-left: 400px;
}

/* line 187, ../../../../scss/email/_default.scss */
td.offset-by-nine {
  padding-left: 450px;
}

/* line 188, ../../../../scss/email/_default.scss */
td.offset-by-ten {
  padding-left: 500px;
}

/* line 189, ../../../../scss/email/_default.scss */
td.offset-by-eleven {
  padding-left: 550px;
}

/* line 191, ../../../../scss/email/_default.scss */
td.sub-offset-by-one {
  padding-left: 5.172413% !important;
}

/* line 192, ../../../../scss/email/_default.scss */
td.sub-offset-by-two {
  padding-left: 13.793102% !important;
}

/* line 193, ../../../../scss/email/_default.scss */
td.sub-offset-by-three {
  padding-left: 22.413791% !important;
}

/* line 194, ../../../../scss/email/_default.scss */
td.sub-offset-by-four {
  padding-left: 31.034480% !important;
}

/* line 195, ../../../../scss/email/_default.scss */
td.sub-offset-by-five {
  padding-left: 39.655169% !important;
}

/* line 196, ../../../../scss/email/_default.scss */
td.sub-offset-by-six {
  padding-left: 48.275858% !important;
}

/* line 197, ../../../../scss/email/_default.scss */
td.sub-offset-by-seven {
  padding-left: 56.896547% !important;
}

/* line 198, ../../../../scss/email/_default.scss */
td.sub-offset-by-eight {
  padding-left: 65.517236% !important;
}

/* line 199, ../../../../scss/email/_default.scss */
td.sub-offset-by-nine {
  padding-left: 74.137925% !important;
}

/* line 200, ../../../../scss/email/_default.scss */
td.sub-offset-by-ten {
  padding-left: 82.758614% !important;
}

/* line 201, ../../../../scss/email/_default.scss */
td.sub-offset-by-eleven {
  padding-left: 91.379303% !important;
}

/* line 203, ../../../../scss/email/_default.scss */
td.expander {
  visibility: hidden;
  width: 0px;
  padding: 0 !important;
}

/* line 209, ../../../../scss/email/_default.scss */
table.columns .text-pad {
  padding-left: 10px;
  padding-right: 10px;
}

/* line 214, ../../../../scss/email/_default.scss */
table.columns .left-text-pad {
  padding-left: 10px;
}

/* line 218, ../../../../scss/email/_default.scss */
table.columns .right-text-pad {
  padding-right: 10px;
}

/* Block Grid */
/* line 224, ../../../../scss/email/_default.scss */
.block-grid {
  width: 100%;
  max-width: 580px;
}

/* line 229, ../../../../scss/email/_default.scss */
.block-grid td {
  display: inline-block;
  padding: 10px;
}

/* line 234, ../../../../scss/email/_default.scss */
.two-up td {
  width: 270px;
}

/* line 238, ../../../../scss/email/_default.scss */
.three-up td {
  width: 173px;
}

/* line 242, ../../../../scss/email/_default.scss */
.four-up td {
  width: 125px;
}

/* line 246, ../../../../scss/email/_default.scss */
.five-up td {
  width: 96px;
}

/* line 250, ../../../../scss/email/_default.scss */
.six-up td {
  width: 76px;
}

/* line 254, ../../../../scss/email/_default.scss */
.seven-up td {
  width: 62px;
}

/* line 258, ../../../../scss/email/_default.scss */
.eight-up td {
  width: 52px;
}

/* Alignment & Visibility Classes */
/* line 264, ../../../../scss/email/_default.scss */
table.center, td.center {
  text-align: center;
}

/* line 273, ../../../../scss/email/_default.scss */
h1.center,
h2.center,
h3.center,
h4.center,
h5.center,
h6.center {
  text-align: center;
}

/* line 277, ../../../../scss/email/_default.scss */
span.center {
  display: block;
  width: 100%;
  text-align: center;
}

/* line 283, ../../../../scss/email/_default.scss */
img.center {
  margin: 0 auto;
  float: none;
}

/* line 289, ../../../../scss/email/_default.scss */
.show-for-small,
.hide-for-desktop {
  display: none !important;
}

/* Typography */
/* line 295, ../../../../scss/email/_default.scss */
body, h1, h2, h3, h4, h5, h6, p {
  color: #222222;
  display: block;
  font-family: "Helvetica", "Arial", sans-serif;
  font-weight: normal;
  padding: 0;
  margin: 0;
  text-align: left;
  line-height: 1.3;
}

/* line 306, ../../../../scss/email/_default.scss */
h1, h2, h3, h4, h5, h6 {
  word-break: normal;
}

/* line 310, ../../../../scss/email/_default.scss */
h1 {
  font-size: 40px;
}

/* line 311, ../../../../scss/email/_default.scss */
h2 {
  font-size: 36px;
}

/* line 312, ../../../../scss/email/_default.scss */
h3 {
  font-size: 32px;
}

/* line 313, ../../../../scss/email/_default.scss */
h4 {
  font-size: 28px;
}

/* line 314, ../../../../scss/email/_default.scss */
h5 {
  font-size: 24px;
}

/* line 315, ../../../../scss/email/_default.scss */
h6 {
  font-size: 20px;
}

/* line 316, ../../../../scss/email/_default.scss */
body, p {
  font-size: 14px;
  line-height: 19px;
}

/* line 318, ../../../../scss/email/_default.scss */
p {
  padding-bottom: 10px;
}

/* line 322, ../../../../scss/email/_default.scss */
small {
  font-size: 10px;
}

/* line 326, ../../../../scss/email/_default.scss */
a {
  color: #2ba6cb;
  text-decoration: none;
}

/* line 331, ../../../../scss/email/_default.scss */
a:hover {
  color: #2795b6 !important;
}

/* line 335, ../../../../scss/email/_default.scss */
a:active {
  color: #2795b6 !important;
}

/* line 339, ../../../../scss/email/_default.scss */
a:visited {
  color: #2ba6cb !important;
}

/* line 348, ../../../../scss/email/_default.scss */
h1 a,
h2 a,
h3 a,
h4 a,
h5 a,
h6 a {
  color: #2ba6cb !important;
}

/* line 357, ../../../../scss/email/_default.scss */
h1 a:active,
h2 a:active,
h3 a:active,
h4 a:active,
h5 a:active,
h6 a:active {
  color: #2ba6cb !important;
}

/* line 366, ../../../../scss/email/_default.scss */
h1 a:visited,
h2 a:visited,
h3 a:visited,
h4 a:visited,
h5 a:visited,
h6 a:visited {
  color: #2ba6cb !important;
}

/* Panels */
/* line 372, ../../../../scss/email/_default.scss */
td.panel {
  background: #f2f2f2;
  border: 1px solid #d9d9d9;
  padding: 10px !important;
}

/* Buttons */
/* line 384, ../../../../scss/email/_default.scss */
table.button,
table.tiny-button,
table.small-button,
table.medium-button,
table.large-button {
  width: 100%;
  overflow: hidden;
}

/* line 393, ../../../../scss/email/_default.scss */
table.button td,
table.tiny-button td,
table.small-button td,
table.medium-button td,
table.large-button td {
  display: block;
  width: auto !important;
  text-align: center;
  font-family: 'Source Sans Pro', Arial, sans-serif;
  border: 1px solid #ebba28;
  background: #ffe400;
  background-color: #f2da0d !important;
  padding: 8px 0 !important;
  border-radius: 5px;
}

/* line 406, ../../../../scss/email/_default.scss */
table.tiny-button td {
  padding: 5px 0 4px;
}

/* line 410, ../../../../scss/email/_default.scss */
table.small-button td {
  padding: 8px 0 7px;
}

/* line 414, ../../../../scss/email/_default.scss */
table.medium-button td {
  padding: 12px 0 10px;
}

/* line 418, ../../../../scss/email/_default.scss */
table.large-button td {
  padding: 21px 0 18px;
}

/* line 426, ../../../../scss/email/_default.scss */
table.button td a,
table.tiny-button td a,
table.small-button td a,
table.medium-button td a,
table.large-button td a {
  font-weight: bold;
  text-decoration: none;
  font-family: Helvetica, Arial, sans-serif;
  color: #333333;
  font-size: 17px;
  display: block;
}

/* line 435, ../../../../scss/email/_default.scss */
table.tiny-button td a {
  font-size: 12px;
  font-weight: normal;
}

/* line 440, ../../../../scss/email/_default.scss */
table.small-button td a {
  font-size: 16px;
}

/* line 444, ../../../../scss/email/_default.scss */
table.medium-button td a {
  font-size: 20px;
}

/* line 448, ../../../../scss/email/_default.scss */
table.large-button td a {
  font-size: 24px;
}

/* line 454, ../../../../scss/email/_default.scss */
table.button:hover td,
table.button:visited td,
table.button:active td {
  background: #2795b6 !important;
}

/* line 460, ../../../../scss/email/_default.scss */
table.button:hover td a,
table.button:visited td a,
table.button:active td a {
  color: #fff !important;
}

/* line 468, ../../../../scss/email/_default.scss */
table.button:hover td,
table.tiny-button:hover td,
table.small-button:hover td,
table.medium-button:hover td,
table.large-button:hover td {
  background: #f2da0d !important;
}

/* line 486, ../../../../scss/email/_default.scss */
table.button:hover td a,
table.button:active td a,
table.button td a:visited,
table.tiny-button:hover td a,
table.tiny-button:active td a,
table.tiny-button td a:visited,
table.small-button:hover td a,
table.small-button:active td a,
table.small-button td a:visited,
table.medium-button:hover td a,
table.medium-button:active td a,
table.medium-button td a:visited,
table.large-button:hover td a,
table.large-button:active td a,
table.large-button td a:visited {
  color: #333 !important;
}

/* line 490, ../../../../scss/email/_default.scss */
table.secondary td {
  background: #e9e9e9;
  border-color: #d0d0d0;
  color: #555;
}

/* line 496, ../../../../scss/email/_default.scss */
table.secondary td a {
  color: #555;
}

/* line 500, ../../../../scss/email/_default.scss */
table.secondary:hover td {
  background: #d0d0d0 !important;
  color: #555;
}

/* line 507, ../../../../scss/email/_default.scss */
table.secondary:hover td a,
table.secondary td a:visited,
table.secondary:active td a {
  color: #555 !important;
}

/* line 511, ../../../../scss/email/_default.scss */
table.success td {
  background: #5da423;
  border-color: #457a1a;
}

/* line 516, ../../../../scss/email/_default.scss */
table.success:hover td {
  background: #457a1a !important;
}

/* line 520, ../../../../scss/email/_default.scss */
table.alert td {
  background: #c60f13;
  border-color: #970b0e;
}

/* line 525, ../../../../scss/email/_default.scss */
table.alert:hover td {
  background: #970b0e !important;
}

/* line 529, ../../../../scss/email/_default.scss */
table.radius td {
  -webkit-border-radius: 3px;
  -moz-border-radius: 3px;
  border-radius: 3px;
}

/* line 535, ../../../../scss/email/_default.scss */
table.round td {
  -webkit-border-radius: 500px;
  -moz-border-radius: 500px;
  border-radius: 500px;
}

/* line 545, ../../../../scss/email/_default.scss */
.button table,
.tiny-button table,
.small-button table,
.medium-button table,
.large-button table {
  width: 100%;
  overflow: hidden;
}

/* line 554, ../../../../scss/email/_default.scss */
.button table td,
.tiny-button table td,
.small-button table td,
.medium-button table td,
.large-button table td {
  display: block;
  width: auto !important;
  text-align: center;
  font-weight: bold;
  text-decoration: none;
  font-family: Helvetica, Arial, sans-serif;
  color: #ffffff;
  background: #2ba6cb;
  border: 1px solid #2284a1;
}

/* line 566, ../../../../scss/email/_default.scss */
.tiny-button table td {
  padding: 5px 10px;
  font-size: 12px;
  font-weight: normal;
}

/* line 573, ../../../../scss/email/_default.scss */
.button table td,
.small-button table td {
  padding: 8px 15px;
  font-size: 16px;
}

/* line 578, ../../../../scss/email/_default.scss */
.medium-button table td {
  padding: 12px 24px;
  font-size: 20px;
}

/* line 583, ../../../../scss/email/_default.scss */
.large-button table td {
  padding: 21px 30px;
  font-size: 24px;
}

/* line 592, ../../../../scss/email/_default.scss */
.button:hover table td,
.tiny-button:hover table td,
.small-button:hover table td,
.medium-button:hover table td,
.large-button:hover table td {
  background: #2795b6 !important;
}

/* line 615, ../../../../scss/email/_default.scss */
.button,
.button:hover,
.button:active,
.button:visited,
.tiny-button,
.tiny-button:hover,
.tiny-button:active,
.tiny-button:visited,
.small-button,
.small-button:hover,
.small-button:active,
.small-button:visited,
.medium-button,
.medium-button:hover,
.medium-button:active,
.medium-button:visited,
.large-button,
.large-button:hover,
.large-button:active,
.large-button:visited {
  color: #ffffff !important;
  font-family: Helvetica, Arial, sans-serif;
  text-decoration: none;
}

/* line 621, ../../../../scss/email/_default.scss */
.secondary table td {
  background: #e9e9e9;
  border-color: #d0d0d0;
}

/* line 626, ../../../../scss/email/_default.scss */
.secondary:hover table td {
  background: #d0d0d0 !important;
}

/* line 630, ../../../../scss/email/_default.scss */
.success table td {
  background: #5da423;
  border-color: #457a1a;
}

/* line 635, ../../../../scss/email/_default.scss */
.success:hover table td {
  background: #457a1a !important;
}

/* line 639, ../../../../scss/email/_default.scss */
.alert table td {
  background: #c60f13;
  border-color: #970b0e;
}

/* line 644, ../../../../scss/email/_default.scss */
.alert:hover table td {
  background: #970b0e !important;
}

/* line 648, ../../../../scss/email/_default.scss */
.radius table td {
  -webkit-border-radius: 3px;
  -moz-border-radius: 3px;
  border-radius: 3px;
}

/* line 654, ../../../../scss/email/_default.scss */
.round table td {
  -webkit-border-radius: 500px;
  -moz-border-radius: 500px;
  border-radius: 500px;
}

/* Outlook First */
/* line 662, ../../../../scss/email/_default.scss */
body.outlook p {
  display: inline !important;
}

/* line 1, ../../../../scss/email/_header.scss */
table.header {
  padding: 10px 0 5px;
}

/* line 4, ../../../../scss/email/_header.scss */
td.header-text {
  text-align: right;
  padding-bottom: 0 !important;
}

/* line 8, ../../../../scss/email/_header.scss */
.header-support-details {
  line-height: 1.2;
  padding-bottom: 0;
  color: #555 !important;
  font-weight: normal;
  font-style: italic;
}
/* line 15, ../../../../scss/email/_header.scss */
.header-support-details strong {
  color: #333;
}

/* line 2, ../../../../scss/email/_email-title.scss */
.email-title-heading,
.upcoming-games-title {
  margin: 0 !important;
  padding: 0 !important;
  line-height: 1;
}

/* line 2, ../../../../scss/email/_navigation.scss */
table.navigation td {
  padding: 0;
}
/* line 5, ../../../../scss/email/_navigation.scss */
table.navigation .navigation-item {
  padding: 14px 0;
  font-family: 'Source Sans Pro', sans-serif;
  border-width: 2px;
  border-right-color: #1bb9fd;
  border-right-style: groove;
}
/* line 12, ../../../../scss/email/_navigation.scss */
table.navigation .three {
  width: 150px;
}

/* line 17, ../../../../scss/email/_navigation.scss */
.navigation-link {
  color: white;
  display: block;
  text-align: center;
  font-size: 16px;
}
/* line 23, ../../../../scss/email/_navigation.scss */
.navigation-link:hover {
  color: white !important;
}

/* line 27, ../../../../scss/email/_navigation.scss */
table.social-link {
  width: 100%;
  vertical-align: middle;
  padding-left: 10px;
  table-layout: fixed;
}
/* line 33, ../../../../scss/email/_navigation.scss */
table.social-link td {
  padding: 7px 0;
  text-align: center;
}

/* line 1, ../../../../scss/email/_upcoming-games.scss */
.game-card {
  background-color: white;
  width: 100%;
  box-shadow: 0 4px 3px -1px rgba(0, 0, 0, 0.3);
}

/* line 6, ../../../../scss/email/_upcoming-games.scss */
.game-card-day {
  background-color: #ebebeb;
  border-radius: 5px 5px 0 0;
  color: #0b196e;
  font-weight: bold;
  font-size: 13px;
  vertical-align: middle;
}
/* line 14, ../../../../scss/email/_upcoming-games.scss */
.game-card-day h6 {
  padding: 2px 0;
}

/* line 19, ../../../../scss/email/_upcoming-games.scss */
.game-card-jackpot,
.game-card-logo {
  vertical-align: middle;
  padding: 12px 10px !important;
}

/* line 24, ../../../../scss/email/_upcoming-games.scss */
.game-card-jackpot h2 {
  text-align: right;
  font-weight: bold;
}

/* line 28, ../../../../scss/email/_upcoming-games.scss */
.game-card-logo {
  width: 100px;
}

/* line 32, ../../../../scss/email/_upcoming-games.scss */
table.game-card td {
  padding-bottom: 0;
}

/* line 36, ../../../../scss/email/_upcoming-games.scss */
.next-draw-card {
  width: 100%;
}

/* line 39, ../../../../scss/email/_upcoming-games.scss */
.next-draw-jackpot {
  font-size: 40px;
  font-weight: bold;
}

/* line 43, ../../../../scss/email/_upcoming-games.scss */
table.game-card .next-draw-logo {
  padding: 10px 0;
}

/* line 46, ../../../../scss/email/_upcoming-games.scss */
.next-draw-logo center {
  min-width: 100px !important;
}

/* line 1, ../../../../scss/email/_way-to-play.scss */
.way-to-play {
  background-color: #0291ce;
  border-radius: 5px;
}

/* line 5, ../../../../scss/email/_way-to-play.scss */
.way-to-play-heading {
  padding-top: 10px;
  padding-bottom: 10px;
}

/* line 9, ../../../../scss/email/_way-to-play.scss */
.way-to-play-method {
  padding-bottom: 0 !important;
}

/* line 14, ../../../../scss/email/_way-to-play.scss */
.way-to-play,
.way-to-play-heading,
.way-to-play-method {
  color: white;
  text-shadow: 1px 1px 2px #015795;
}

/* line 19, ../../../../scss/email/_way-to-play.scss */
.way-to-play-method {
  padding-bottom: 10px !important;
}

/* line 22, ../../../../scss/email/_way-to-play.scss */
.way-to-play-text {
  line-height: 15px !important;
  font-size: 13px;
  text-align: center;
  color: white;
}

/* line 1, ../../../../scss/email/_footer.scss */
table.footer {
  height: 38px;
}

/* line 4, ../../../../scss/email/_footer.scss */
.pre-footer-image {
  padding: 15px 0 22px;
}

/* line 7, ../../../../scss/email/_footer.scss */
.pre-footer-text {
  font-size: 11px;
  line-height: 1.5;
}

/* line 11, ../../../../scss/email/_footer.scss */
.footer-nav {
  padding: 10px;
}

/* line 14, ../../../../scss/email/_footer.scss */
.footer-link {
  color: white;
  font-family: 'Source Sans Pro', Arial, sans-serif;
  font-size: 12px;
}

/* line 19, ../../../../scss/email/_footer.scss */
table.unsubscribe {
  height: 80px;
}

/* line 22, ../../../../scss/email/_footer.scss */
.unsubscribe-content {
  line-height: 1.3;
  font-size: 10px;
}

/* line 26, ../../../../scss/email/_footer.scss */
.unsubscribe-link {
  font-family: 'Source Sans Pro', Arial, sans-serif;
  color: #333;
  font-size: 10px;
  line-height: 12px;
}

/* line 32, ../../../../scss/email/_footer.scss */
.pre-footer center {
  min-width: 100px !important;
}

/* line 35, ../../../../scss/email/_footer.scss */
.mobile-support {
  padding-left: 15px;
}

/* line 38, ../../../../scss/email/_footer.scss */
.mobile-support-text {
  font-size: 11px;
}

/* line 41, ../../../../scss/email/_footer.scss */
.table-cell-small {
  display: none !important;
}

/* line 5, ../../../../scss/email/_button.scss */
table.button td,
table.tiny-button td,
table.small-button td,
table.medium-button td,
table.large-button td {
  display: block;
  width: auto !important;
  text-align: center;
  font-family: 'Source Sans Pro', Arial, sans-serif;
  border: 1px solid #ebba28;
  background: #ffe400;
  padding: 8px 0 !important;
  background-image: -webkit-gradient(linear, 50% 0%, 50% 100%, color-stop(0%, #ffe400), color-stop(100%, #ffc000));
  background-image: -webkit-linear-gradient(#ffe400, #ffc000);
  background-image: -moz-linear-gradient(#ffe400, #ffc000);
  background-image: -o-linear-gradient(#ffe400, #ffc000);
  background-image: linear-gradient(#ffe400, #ffc000);
  background-color: #f2da0d;
  box-shadow: inset 0 2px 0 -1px rgba(255, 255, 255, 0.6);
  border-radius: 5px;
}

/* line 24, ../../../../scss/email/_button.scss */
table.tiny-button td {
  padding: 5px 0 4px;
}

/* line 28, ../../../../scss/email/_button.scss */
table.small-button td {
  padding: 8px 0 7px;
}

/* line 32, ../../../../scss/email/_button.scss */
table.medium-button td {
  padding: 12px 0 10px;
}

/* line 36, ../../../../scss/email/_button.scss */
table.large-button td {
  padding: 21px 0 18px;
}

/* line 44, ../../../../scss/email/_button.scss */
table.button td a,
table.tiny-button td a,
table.small-button td a,
table.medium-button td a,
table.large-button td a {
  font-weight: bold;
  text-decoration: none;
  font-family: Helvetica, Arial, sans-serif;
  color: #333333;
  font-size: 17px;
  display: block;
}

/* line 53, ../../../../scss/email/_button.scss */
table.tiny-button td a {
  font-size: 12px;
  font-weight: normal;
}

/* line 58, ../../../../scss/email/_button.scss */
table.small-button td a {
  font-size: 16px;
}

/* line 62, ../../../../scss/email/_button.scss */
table.medium-button td a {
  font-size: 20px;
}

/* line 66, ../../../../scss/email/_button.scss */
table.large-button td a {
  font-size: 24px;
}

/* line 72, ../../../../scss/email/_button.scss */
table.button:hover td,
table.button:visited td,
table.button:active td {
  background: #2795b6 !important;
}

/* line 78, ../../../../scss/email/_button.scss */
table.button:hover td a,
table.button:visited td a,
table.button:active td a {
  color: #fff !important;
}

/* line 86, ../../../../scss/email/_button.scss */
table.button:hover td,
table.tiny-button:hover td,
table.small-button:hover td,
table.medium-button:hover td,
table.large-button:hover td {
  background: #f2da0d !important;
}

/* line 104, ../../../../scss/email/_button.scss */
table.button:hover td a,
table.button:active td a,
table.button td a:visited,
table.tiny-button:hover td a,
table.tiny-button:active td a,
table.tiny-button td a:visited,
table.small-button:hover td a,
table.small-button:active td a,
table.small-button td a:visited,
table.medium-button:hover td a,
table.medium-button:active td a,
table.medium-button td a:visited,
table.large-button:hover td a,
table.large-button:active td a,
table.large-button td a:visited {
  color: #333 !important;
}

/* line 14, ../../../../scss/email/ink.scss */
h1 {
  font-size: 26px;
}

/* line 15, ../../../../scss/email/ink.scss */
h2 {
  font-size: 22px;
}

/* line 16, ../../../../scss/email/ink.scss */
h3 {
  font-size: 18px;
}

/* line 17, ../../../../scss/email/ink.scss */
h4 {
  font-size: 17px;
}

/* line 18, ../../../../scss/email/ink.scss */
h5 {
  font-size: 16px;
}

/* line 19, ../../../../scss/email/ink.scss */
h6 {
  font-size: 14px;
}

/* line 21, ../../../../scss/email/ink.scss */
h1, h2, h3, h4, h5, h6 {
  font-family: 'Source Sans Pro', Arial, sans-serif;
  font-weight: 600;
  color: #0b196e;
}
/* line 25, ../../../../scss/email/ink.scss */
h1.alt, h2.alt, h3.alt, h4.alt, h5.alt, h6.alt {
  color: #222;
}

/* line 29, ../../../../scss/email/ink.scss */
a {
  color: #00b8ed;
}

/* line 35, ../../../../scss/email/ink.scss */
body,
p,
.page-content {
  font-family: Arial, sans-serif;
  font-size: 12px;
  line-height: 19px;
}

/* line 41, ../../../../scss/email/ink.scss */
.body {
  background-color: #ebebeb;
}

/* line 47, ../../../../scss/email/ink.scss */
.navigation,
.way-to-play,
.content,
.secondary-bg {
  box-shadow: 0 4px 2px -2px rgba(0, 0, 0, 0.1);
}

/* line 52, ../../../../scss/email/ink.scss */
.navigation,
.way-to-play {
  background-color: #0291ce;
  background-image: -webkit-gradient(linear, 50% 0%, 50% 100%, color-stop(0%, #00b8ed), color-stop(100%, #0291ce));
  background-image: -webkit-linear-gradient(#00b8ed, #0291ce);
  background-image: -moz-linear-gradient(#00b8ed, #0291ce);
  background-image: -o-linear-gradient(#00b8ed, #0291ce);
  background-image: linear-gradient(#00b8ed, #0291ce);
}

/* line 60, ../../../../scss/email/ink.scss */
.secondary-bg {
  background-color: #0b196e;
  background-image: -webkit-gradient(linear, 50% 0%, 50% 100%, color-stop(0%, #0b196e), color-stop(100%, #0d1e85));
  background-image: -webkit-linear-gradient(#0b196e, #0d1e85);
  background-image: -moz-linear-gradient(#0b196e, #0d1e85);
  background-image: -o-linear-gradient(#0b196e, #0d1e85);
  background-image: linear-gradient(#0b196e, #0d1e85);
}

/* line 69, ../../../../scss/email/ink.scss */
.content {
  background-color: white;
}

/* line 72, ../../../../scss/email/ink.scss */
.footer {
  background-color: #222;
  color: white;
}

/* line 76, ../../../../scss/email/ink.scss */
.unsubscribe {
  background-color: #fff;
}

/* line 82, ../../../../scss/email/ink.scss */
.secondary-bg,
.navigation,
.game-card,
.content {
  border-radius: 8px;
}

/* line 86, ../../../../scss/email/ink.scss */
td[height="10"] {
  font-size: 10px !important;
  line-height: 10px !important;
}

/* line 90, ../../../../scss/email/ink.scss */
td[height="2"] {
  font-size: 2px !important;
  line-height: 2px !important;
}

/* line 96, ../../../../scss/email/ink.scss */
.fixed-width {
  table-layout: fixed;
  width: 100%;
}

/* line 100, ../../../../scss/email/ink.scss */
.primary-color {
  color: #00b8ed;
}

/* line 103, ../../../../scss/email/ink.scss */
.no-lh {
  line-height: 1 !important;
}

/* line 106, ../../../../scss/email/ink.scss */
.no-padding-bottom {
  padding-bottom: 0 !important;
}

/* line 109, ../../../../scss/email/ink.scss */
.padding-bottom {
  padding-bottom: 10px !important;
}

/* line 112, ../../../../scss/email/ink.scss */
.white {
  color: white;
}

/* line 115, ../../../../scss/email/ink.scss */
.vertical-middle {
  vertical-align: middle;
}

/* line 118, ../../../../scss/email/ink.scss */
.text-center {
  text-align: center;
}

/* line 121, ../../../../scss/email/ink.scss */
.text-right {
  text-align: right;
}

/* line 124, ../../../../scss/email/ink.scss */
.text-right-large-center-small {
  text-align: right;
}

/* line 127, ../../../../scss/email/ink.scss */
.image-center {
  text-align: center !important;
  display: block !important;
  margin: 0 auto !important;
}
HERE;

    }
}
