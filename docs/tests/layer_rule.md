
# Layer rules test suite



##  Layer rules test suite / Sanity test


  ![](lr1.png)  

```  
Layer_A contains P1, P2
Layer_B contains P3, P4
Layer_A is a layer over Layer_B
```  


  Code is compliant with rules file, no output expected  


Layer rules test suite / Sanity test [Successful](layer_rule.md#layer-rules-test-suite--sanity-test)

##  Layer rules test suite / Batik project architecture

  Architecture of the The Apache Batik Project, available <here at https://xmlgraphics.apache.org/batik/using/architecture.html>.  

  ![](batik.png)  

  Described by this rules file :  

```  
Applications      contains Browser and Rasterizer
Core_Modules      contains UI_Component, Transcoder, SVG_Generator, Bridge and SVGDOM
Low_Level_Modules contains Renderer, GVT and SVG_Parser

Applications is a layer over Core_Modules
Core_Modules is a layer over Low_Level_Modules
```  


  Code is compliant with rules file, no output expected  


Layer rules test suite / Batik project architecture [Successful](layer_rule.md#layer-rules-test-suite--batik-project-architecture)

##  Layer rules test suite / Reverse dependency test

  Detection of a dependancy from a lower layer component to an upper layer component.  

  ![](lr2.png)  

```  
Layer_A contains P1, P2, P5
Layer_B contains P3, P4
Layer_A is a layer over Layer_B
```  

  Expected output :  

```  
Error : dir2/p4.ads:1: P4 is in Layer_B layer, and so shall not use P5 in the upper Layer_A layer
```  


Layer rules test suite / Reverse dependency test [Successful](layer_rule.md#layer-rules-test-suite--reverse-dependency-test)

##  Layer rules test suite / Layer bridging test

  Detection of a dependancy link crossing a layer.  

  ![](lr3.png)  

```  
Layer_A contains P1, P2
Layer_B contains P3, P4
Layer_A is a layer over Layer_B
```  

  Expected output :  

```  
Warning : dir3c/p6.adb:1: P6 is neither in Layer_A or Layer_B layer, and so shall not directly use P4 in the Layer_B layer
```  


Layer rules test suite / Layer bridging test [Successful](layer_rule.md#layer-rules-test-suite--layer-bridging-test)

##  Layer rules test suite / Using a package that is neither in the same layer, nor in the visible layer

  Detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer.  

  ![](lr4.png)  

```  
Layer_A contains P1, P2
Layer_B contains P3, P4
Layer_A is a layer over Layer_B
```  

  Expected output :  

```  
Warning : dir4/p2.ads:3: P2 (in Layer_A layer) uses P7 that is neither in the same layer, nor in the lower Layer_B layer
```  


Layer rules test suite / Using a package that is neither in the same layer, nor in the visible layer [Successful](layer_rule.md#layer-rules-test-suite--using-a-package-that-is-neither-in-the-same-layer-nor-in-the-visible-layer)
