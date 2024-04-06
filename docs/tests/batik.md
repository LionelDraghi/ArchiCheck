
# Batik test suite



##  Batik test suite / --list_file test

  > archicheck -lf -Ir ./batik-1.9  
  Expected (1658 files) :  

```  
./batik-1.9/batik-anim/src/main/java/org/apache/batik/anim/AbstractAnimation.java
./batik-1.9/batik-anim/src/main/java/org/apache/batik/anim/AnimationEngine.java
./batik-1.9/batik-anim/src/main/java/org/apache/batik/anim/AnimationException.java
...
./batik-1.9/test-sources/org/apache/batik/transcoder/wmf/Messages.java
./batik-1.9/test-sources/org/apache/batik/transcoder/wmf/WMFAccuracyTest.java
./batik-1.9/test-sources/org/apache/batik/util/ApplicationSecurityEnforcerTest.java
```  


Batik test suite / --list_file test [Successful](tests_status.md#successful)

##  Batik test suite / public class


```  
package org.apache.batik.apps.jsvg;

import javax.swing.JFrame;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.svg.SVGUserAgentGUIAdapter;

/**
 * Simplest "complete" SVG Viewer using Batik.
 *
 * This is about as simple as an SVG viewer application can get.
 * It shuts it's self down when all windows are closed.
 * It reports errors interactively, and it takes a list of URI's
 * to open.
 *
 * @author <a href="mailto:Thomas.DeWeese@Kodak.com">deweese</a>
 * @version $Id: JSVG.java 1733420 2016-03-03 07:41:59Z gadams $
 */
public class JSVG extends JFrame{
```  


  > archicheck -ld -I dir2  

  Expected :  

```  
org.apache.batik.apps.jsvg.JSVG class depends on javax.swing.JFrame
org.apache.batik.apps.jsvg.JSVG class depends on java.awt.BorderLayout
org.apache.batik.apps.jsvg.JSVG class depends on java.awt.Dimension
org.apache.batik.apps.jsvg.JSVG class depends on java.awt.event.WindowAdapter
org.apache.batik.apps.jsvg.JSVG class depends on java.awt.event.WindowEvent
org.apache.batik.apps.jsvg.JSVG class depends on org.apache.batik.swing.JSVGCanvas
org.apache.batik.apps.jsvg.JSVG class depends on org.apache.batik.swing.svg.SVGUserAgentGUIAdapter
```  


Batik test suite / public class [Successful](tests_status.md#successful)

##  Batik test suite / public interface class


```  
package org.apache.batik.dom.events;

import org.w3c.dom.DOMException;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventException;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;

/**
 * A Node that uses an EventSupport for its event registration and
 * dispatch.
 *
 * @author <a href="mailto:Thierry.Kormann@sophia.inria.fr">Thierry Kormann</a>
 * @author <a href="mailto:stephane@hillion.org">Stephane Hillion</a>
 * @version $Id: NodeEventTarget.java 1733416 2016-03-03 07:07:13Z gadams $
 */
public interface NodeEventTarget extends EventTarget {
```  


  > archicheck -ld -I dir3  

  Expected :  

```  
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.DOMException
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.Event
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.EventException
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.EventListener
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.EventTarget
```  


Batik test suite / public interface class [Successful](tests_status.md#successful)

##  Batik test suite / no import


```  
package org.apache.batik.dom.util;

/**
 * This class represents a triply indexed hash table.
 * <br>Note: This implementation is not Thread-safe.
 *
 * @author <a href="mailto:stephane@hillion.org">Stephane Hillion</a>
 * @version $Id: TriplyIndexedTable.java 1733416 2016-03-03 07:07:13Z gadams $
 */
public class TriplyIndexedTable {
```  

  > archicheck -ld -I dir4  

  Expected :  

```  
```  


Batik test suite / no import [Successful](tests_status.md#successful)

##  Batik test suite / no package


```  
import org.w3c.dom.DOMException;
import org.w3c.dom.events.Event;

public interface NodeEventTarget extends EventTarget {
```  

  > archicheck -ld -I dir5  

  Expected :  

```  
NodeEventTarget interface depends on org.w3c.dom.DOMException
NodeEventTarget interface depends on org.w3c.dom.events.Event
```  


Batik test suite / no package [Successful](tests_status.md#successful)

##  Batik test suite / public abstract class


  This class is in transcoder, and uses Bridge and GVT, and that's OK  

```  
package org.apache.batik.transcoder;

import java.awt.Dimension;
import java.awt.geom.AffineTransform;
import java.awt.geom.Dimension2D;
import java.awt.geom.Rectangle2D;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.batik.anim.dom.SAXSVGDocumentFactory;
import org.apache.batik.anim.dom.SVGDOMImplementation;
import org.apache.batik.anim.dom.SVGOMDocument;
import org.apache.batik.bridge.BaseScriptingEnvironment;
import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.BridgeException;
import org.apache.batik.bridge.DefaultScriptSecurity;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.NoLoadScriptSecurity;
import org.apache.batik.bridge.RelaxedScriptSecurity;
import org.apache.batik.bridge.SVGUtilities;
import org.apache.batik.bridge.ScriptSecurity;
import org.apache.batik.bridge.UserAgent;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.bridge.ViewBox;
import org.apache.batik.bridge.svg12.SVG12BridgeContext;
import org.apache.batik.dom.util.DOMUtilities;
import org.apache.batik.dom.util.DocumentFactory;
import org.apache.batik.gvt.CanvasGraphicsNode;
import org.apache.batik.gvt.CompositeGraphicsNode;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.transcoder.keys.BooleanKey;
import org.apache.batik.transcoder.keys.FloatKey;
import org.apache.batik.transcoder.keys.LengthKey;
import org.apache.batik.transcoder.keys.Rectangle2DKey;
import org.apache.batik.transcoder.keys.StringKey;
import org.apache.batik.util.ParsedURL;
import org.apache.batik.util.SVGConstants;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.svg.SVGSVGElement;

/**
 * This class may be the base class of all transcoders which take an
 * SVG document as input and which need to build a DOM tree. The
 * <code>SVGAbstractTranscoder</code> uses several different hints that
 * guide it's behaviour:<br/>
 *
 * <ul>
 *   <li><code>KEY_WIDTH, KEY_HEIGHT</code> can be used to specify how to scale the
 *       SVG image</li>
 * </ul>
 *
 * @author <a href="mailto:Thierry.Kormann@sophia.inria.fr">Thierry Kormann</a>
 * @version $Id: SVGAbstractTranscoder.java 1733416 2016-03-03 07:07:13Z gadams $
 */
public abstract class SVGAbstractTranscoder extends XMLAbstractTranscoder {
```  

  > archicheck rules.B -q -I dir6  
  No output expected  

Batik test suite / public abstract class [Successful](tests_status.md#successful)

##  Batik test suite / Let's add dependencies to Browser and Rasterizer into a Transcoder class


```  
package org.apache.batik.transcoder;

import org.w3c.dom.Browser.Event;
import org.apache.batik.apps.Rasterizer;

public interface NodeEventTarget extends EventTarget {

}
```  

  Rules:  
```  
Applications      contains org.apache.batik.apps.rasterizer
Core_Modules      contains org.apache.batik.transcoder

Applications is a layer over Core_Modules
```  

  Run:  
  > archicheck rules.7 -I dir7  

  Expected:  
```  
Error : dir7/MyClass.java:4: org.apache.batik.transcoder.NodeEventTarget is in Core_Modules layer, and so shall not use org.apache.batik.apps.Rasterizer in the upper Applications layer
```  


Batik test suite / Let's add dependencies to Browser and Rasterizer into a Transcoder class [Successful](tests_status.md#successful)

##  Batik test suite / -ld test

  > archicheck -ld -Ir ./batik-1.9 | sort  

  10717 dependencies expected :  

```  
AppletDemo class depends on java.io.IOException
AppletDemo class depends on java.net.URL
AppletDemo class depends on javax.swing.JApplet
AppletDemo class depends on org.apache.batik.dom.svg.SAXSVGDocumentFactory
AppletDemo class depends on org.apache.batik.swing.JSVGCanvas
AppletDemo class depends on org.apache.batik.util.XMLResourceDescriptor
AppletDemo class depends on org.w3c.dom.Document
AppletDemo class depends on org.w3c.dom.Element
AppletDemo class depends on org.w3c.dom.Node
com.test.script.EventListenerInitializerImpl class depends on org.w3c.dom.Element
...
org.test.ScrollExample class depends on javax.swing.WindowConstants
org.test.ScrollExample class depends on org.apache.batik.swing.*
org.w3c.dom.events.DocumentEvent interface depends on org.w3c.dom.DOMException
org.w3c.dom.events.EventTarget interface depends on org.w3c.dom.DOMException
org.w3c.dom.events.KeyboardEvent interface depends on org.w3c.dom.views.AbstractView
org.w3c.dom.events.MouseEvent interface depends on org.w3c.dom.views.AbstractView
org.w3c.dom.events.MutationEvent interface depends on org.w3c.dom.Node
org.w3c.dom.events.MutationNameEvent interface depends on org.w3c.dom.Node
org.w3c.dom.events.TextEvent interface depends on org.w3c.dom.views.AbstractView
org.w3c.dom.events.UIEvent interface depends on org.w3c.dom.views.AbstractView
```  


Batik test suite / -ld test [Successful](tests_status.md#successful)
