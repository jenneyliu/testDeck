---
title       : Body Mass Index (BMI) Calculator
subtitle    : A shiny application
author      : Jenny Liu
job         : Data Scintist
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides

---



<br/>
<br/>
## <font size="10"> Are you in good shape?  </font>
<br/>
<br/>

<font size="4.5"> Some most common questions about weight: </font>


<br/>
<font size="5"> Should I lose weight? </font>

<br/>
<font size="5"> Is my weight normal? </font>

<br/>
<font size="5"> How much should I weigh for my height? </font>


--- .class #id 


## Body Mass Index (BMI)

<br/>
### What is BMI
1. Your BMI is a measure that relates your weight to your height. 
2. To calculate BMI, divide your weight (kilograms) by the square of your height (meters).



### Interpretation of BMI


<table style="width:400px; boder:1px; margin-top:0.3cm; padding-top:0.3cm; font-size=4">
<tr>
  <td> BMI &#x2264 18.5 </td>
  <td> Underweight</td>  
</tr>

<tr>
  <td>18.5 < BMI &#x2264 25</td>
  <td> Ideal</td>  
</tr>

<tr>
  <td>25 < BMI &#x2264 30 </td>
  <td> Overweight</td>  
</tr>

<tr>
  <td> 30 < BMI </td>
  <td> Obese</td>  
</tr>
</table>








---



## An example




<font size="4">
For example,  if $weight=80$ kilograms and $height=1.8$ meters, then  $BMI = weight / (height^2) = 24.6914$
</font>

![plot of chunk unnamed-chunk-2](assets/fig/unnamed-chunk-2.png) 

<font size="4">
Since the $BMI < 25$,  given the height the weight is <b> ideal </b>.
</font>



---


## Body Mass Index (BMI) Calculator

<br/>
Body Mass Index (BMI) Calculator is a shiny application. After you enter your weight and height, this application will calculate your BMI and tell you if you are in good shape or not.

<br/>
1. The link to this shiny application:
<a href="https://jenneyliu.shinyapps.io/Project/" target="_blank">BodyMassIndex (BMI) Calculator</a>

<br/>
2. The link to the Supporting Documentation:
<a href="https://jenneyliu.shinyapps.io/Project/readme.html" target="_blank">Supporting Documentation</a>

<br/>
<br/>
<font size="6"> Stay in shape,  stay healthy! </font>







