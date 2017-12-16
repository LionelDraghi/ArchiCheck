
# Java code test suite



##  Java code test suite / --list_file test, recursive and non recursive

  > archicheck -lf -I ./batik-1.9/test-sources/org/apache/batik/dom
  Expected :

  ```
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/ReplaceChildTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/ElementTraversalTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeTextContentTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/HasChildNodesTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/GetElementsByTagNameNSTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/RemoveAttributeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/TextWholeTextTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DOMUtilitiesCharacterEscaping.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/AttrIsIdTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DocumentRenameNodeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeCompareDocumentPositionTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/CloneElementTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/EcmaScriptDOMTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/AppendChildTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/ElementSetIdAttributeNSTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/SerializationTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DocumentNormalizeDocumentTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/EventTargetAddEventListenerNSTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/SetAttributeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NullNamespaceTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeGetUserDataTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DOM3Test.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/TextReplaceWholeTextTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeBaseURITest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DocumentAdoptNodeTest.java
  ```

  > archicheck -lf -r -I ./batik-1.9/test-sources/org/apache/batik/dom
  Expected :

  ```
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/ReplaceChildTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/ElementTraversalTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeTextContentTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/HasChildNodesTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/GetElementsByTagNameNSTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/RemoveAttributeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/TextWholeTextTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DOMUtilitiesCharacterEscaping.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/AttrIsIdTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DocumentRenameNodeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeCompareDocumentPositionTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/CloneElementTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/EcmaScriptDOMTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/AppendChildTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/ElementSetIdAttributeNSTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/SerializationTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DocumentNormalizeDocumentTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/EventTargetAddEventListenerNSTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/SetAttributeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NullNamespaceTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeGetUserDataTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DOM3Test.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/TextReplaceWholeTextTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/NodeBaseURITest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/DocumentAdoptNodeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/svg/EcmaScriptSVGDOMTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/svg/ImportNodeTest.java
/home/lionel/Proj/Archicheck/Tests/11_Batik/batik-1.9/test-sources/org/apache/batik/dom/svg/CloneNodeTest.java
  ```


 Java code test suite / --list_file test, recursive and non recursive [Successful]

##  Java code test suite / simple import 


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


  > archicheck -lf -I dir2

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


 Java code test suite / simple import [Successful]

##  Java code test suite / interface


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


  > archicheck -lf -I dir3

  Expected :

  ```
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.DOMException 
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.Event 
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.EventException 
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.EventListener 
org.apache.batik.dom.events.NodeEventTarget interface depends on org.w3c.dom.events.EventTarget 
  ```


 Java code test suite / interface [Successful]

##  Java code test suite / no import


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

  > archicheck -lf -I dir4

  Expected :

  ```
  ```


 Java code test suite / no import [Successful]

##  Java code test suite / no package


  ```
import org.w3c.dom.DOMException;
import org.w3c.dom.events.Event;

public interface NodeEventTarget extends EventTarget {
  ```

  > archicheck -lf -I dir5

  Expected :

  ```
NodeEventTarget interface depends on org.w3c.dom.DOMException 
NodeEventTarget interface depends on org.w3c.dom.events.Event 
  ```


 Java code test suite / no package [Successful]

##  Java code test suite / real Batik code


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

  > archicheck rules.B -I dir6
  No output expected

 Java code test suite / real Batik code [Empty]

##  Java code test suite / Let's add dependencies to Browser and Rasterizer into a Transcoder class


  ```
package org.apache.batik.transcoder;

import org.w3c.dom.Browser.Event;
import org.w3c.dom.events.Rasterizer;

public interface NodeEventTarget extends EventTarget {

}
  ```

  > archicheck rules.B -I dir7

  ```

  ```


 Java code test suite / Let's add dependencies to Browser and Rasterizer into a Transcoder class [Empty]
