## Feature: Batik test suite

### Scenario: --list_file test

> acc -lf -Ir ./batik-1.9

Expected files:

```
... (many Java files)
```

- Given there is no `batik-1.9` directory
- Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully
- When I run `./acc -lf -Ir ./batik-1.9`
- Then I get file (unordered) `11_Batik/expected_output.1`

### Scenario: public class

```
package org.apache.batik.contrib.jsvg;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class JSVG
```

> acc -ld -I dir2

Expected :

```
dir2/JSVG.java:1: JSVG depends on Document
dir2/JSVG.java:2: JSVG depends on Element
```

- Given there is no `dir2` directory
- Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully
- Given I run `mkdir -p dir2` Successfully
- Given I run `cp ./batik-1.9/contrib/jsvg/JSVG.java dir2` Successfully
- When I run `./acc -ld -I dir2`
- Then I get file `11_Batik/expected_output.2`

### Scenario: public interface class

```
package org.apache.batik.dom.events;

import org.w3c.dom.events.EventTarget;

public interface NodeEventTarget extends EventTarget {
```

> acc -ld -I dir3

Expected :

```
dir3/NodeEventTarget.java:1: NodeEventTarget depends on EventTarget
```

- Given there is no `dir3` directory
- Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully
- Given I run `mkdir -p dir3` Successfully
- Given I run `cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/events/NodeEventTarget.java dir3` Successfully
- When I run `./acc -ld -I dir3`
- Then I get file `11_Batik/expected_output.3`

### Scenario: no import

```
package org.apache.batik.dom.util;

public class TriplyIndexedTable {
```

> acc -ld -I dir4

Expected :

```
No dependencies
```

- Given there is no `dir4` directory
- Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully
- Given I run `mkdir -p dir4` Successfully
- Given I run `cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/util/TriplyIndexedTable.java dir4` Successfully
- When I run `./acc -ld -I dir4`
- Then I get file `11_Batik/expected_output.4`

### Scenario: no package

```
import org.w3c.dom.DOMException;
import org.w3c.dom.events.Event;

public interface NodeEventTarget extends EventTarget {

}
```

> acc -ld -I dir5

Expected :

```
dir5/MyClass.java:1: MyClass depends on DOMException
dir5/MyClass.java:2: MyClass depends on Event
```

- Given there is no `dir5` directory
- Given I run `mkdir -p dir5` Successfully
- Given the file `dir5/MyClass.java`
```java
import org.w3c.dom.DOMException;
import org.w3c.dom.events.Event;

public interface NodeEventTarget extends EventTarget {

}
```
- When I run `./acc -ld -I dir5`
- Then I get file `11_Batik/expected_output.5`

### Scenario: public abstract class

This class is in transcoder, and uses Bridge and GVT, and that's OK

```
package org.apache.batik.transcoder;

import org.apache.batik.bridge.Bridge;
import org.apache.batik.gvt.GVT;

public abstract class SVGAbstractTranscoder {
```

> acc 11_Batik/rules.B -q -I dir6

No output expected

- Given there is no `dir6` directory
- Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully
- Given I run `mkdir -p dir6` Successfully
- Given I run `cp ./batik-1.9/batik-transcoder/src/main/java/org/apache/batik/transcoder/SVGAbstractTranscoder.java dir6` Successfully
- Given there is a file `11_Batik/rules.B`
- When I run `./acc 11_Batik/rules.B -q -I dir6`
- Then I get file `11_Batik/expected_output.6`

### Scenario: Let's add dependencies to Browser and Rasterizer into a Transcoder class

```java
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
> acc rules.7 -I dir7

Expected:

```
Error : dir7/MyClass.java:4: org.apache.batik.transcoder.NodeEventTarget is in Core_Modules layer, and so shall not use org.apache.batik.apps.Rasterizer in the upper Applications layer
```

- Given there is no `dir7` directory
- Given I run `mkdir -p dir7` Successfully
- Given the file `dir7/MyClass.java`
```java
package org.apache.batik.transcoder;

import org.w3c.dom.Browser.Event;
import org.apache.batik.apps.Rasterizer;

public interface NodeEventTarget extends EventTarget {

}
```
- Given the file `rules.7`
```
Applications      contains org.apache.batik.apps.rasterizer
Core_Modules      contains org.apache.batik.transcoder

Applications is a layer over Core_Modules
```
- When I run `./acc rules.7 -I dir7`
- Then I get file `11_Batik/expected_output.7`

### Scenario: -ld test

> acc -ld -Ir ./batik-1.9

- Given there is no `batik-1.9` directory
- Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully
- When I run `./acc -ld -Ir ./batik-1.9`
- Then I get file (unordered) `11_Batik/expected_output.8`
