Version 1.5.2. Use the Issues page to report bugs or send them directly to lasse@bonecode.com.

<b>Note!</b> Current trunk should never be considered as a stable version. Use the latest <a href="https://github.com/bonecode/TBCEditor/releases">release</a> version instead. 

<h3>Description</h3>

A syntax highlighting edit control for RAD Studio (Delphi/C++ Builder) with code folding, completion proposal, matching pair, minimap, sync edit, word wrap, etc. External highlighter and color scheme files are in JSON format which can be also loaded from a stream.

<h3>Build requirements</h3>

* <a href="https://github.com/ahausladen/JsonDataObjects">Json Data Objects</a> (included)
* Delphi versions XE4, XE5, XE6, XE7, XE8, Seattle, and Berlin are supported 
* Delphi XE7: Update 1 required
* C++ Builder versions XE7, XE8, and Seattle are supported

<b>Note!</b> Do not offer me older versions to support. I don't have time to keep up with those.

<h3>Conditional compilation</h3>

Define | Description 
--- | --- 
USE_ALPHASKINS | Use <a href="http://www.alphaskins.com/">AlphaSkins</a>. AlphaSkins are most powerful theming solutions for apps developed in Delphi.
USE_VCL_STYLES | Use VCL styles. A set of graphical details that define the look and feel of a VCL application.

<h3>Usage example</h3>

```objectpascal
  with BCEditor1 do 
  begin
    Highlighter.LoadFromFile('JSON.json');
    Highlighter.Colors.LoadFromFile('Default.json'); 
    LoadFromFile(GetHighlighterFileName('JSON.json')); 
    ...
    Lines.Text := Highlighter.Info.General.Sample; 
  end;
```
<b>Note!</b> LoadFromStream does not support multi-highlighters (for example HTML with Scripts.json). Override TBCBaseEditor.CreateFileStream function, if you want to load multi-highlighters from a stream. 

<h3>Demo</h3>

TBCEditor Control Demo v. 1.5.2. 

  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo32.zip">32-bit Windows</a>
  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo64.zip">64-bit Windows</a>

The latest update: 21.03.2016 17:33, UTC+02:00

Demo source build requires 

- <a href="http://www.alphaskins.com/">AlphaSkins</a>  
- <a href="http://www.ehlib.com/">EhLib</a>
- <a href="https://github.com/bonecode/Common">Common</a>

<h3>Documentation</h3>

Documentation will be written after the project stabilizes and dust settles. This project is developed in my spare time without sources of income and as long as this is the case there is no timetable for anything. 

<h3>Projects using the control</h3>

* <a href="http://www.bonecode.com">EditBone</a>
* <a href="http://www.mitec.cz/ibq.html">MiTeC Interbase Query</a>
* <a href="http://www.mitec.cz/sqliteq.html">MiTeC SQLite Query</a>

<h3>Screenshots</h3>

![bceditor1](https://cloud.githubusercontent.com/assets/11475177/11452990/a3774372-9602-11e5-8a0b-7ad2b568e4b2.png)
![bceditor0](https://cloud.githubusercontent.com/assets/11475177/11832901/3ac6cfc4-a3c6-11e5-984e-2e174beacd74.png)
![bceditor2](https://cloud.githubusercontent.com/assets/11475177/11452991/a3785e88-9602-11e5-801c-d8e9a7b8ab64.png)
![bceditor3](https://cloud.githubusercontent.com/assets/11475177/11452992/a37b154c-9602-11e5-882c-5a73809be517.png)
![bceditor4](https://cloud.githubusercontent.com/assets/11475177/11452987/a36de61a-9602-11e5-80e9-abd797af7a71.png)
![bceditor5](https://cloud.githubusercontent.com/assets/11475177/11452988/a3716a2e-9602-11e5-994b-0934bb8e5a76.png)
![bceditor6](https://cloud.githubusercontent.com/assets/11475177/11452989/a375d938-9602-11e5-8cbf-103f6a44db13.png)
![bceditor7](https://cloud.githubusercontent.com/assets/11475177/12049668/bb345a32-aef2-11e5-8a4e-715c26d154f6.png)








