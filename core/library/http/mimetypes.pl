:- module(mimetypes, [], [assertions, isomodes, dcg, doccomments]).

%! \title MIME types
%  \module 
%
%  A selection of important [MIME types](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types)

:- export(mimetype/3).
mimetype('.aac', 'audio', 'aac'). % AAC audio file
mimetype('.abw', 'application', 'x-abiword'). % AbiWord document
mimetype('.arc', 'application', 'octet-stream'). % Archive document (multiple files embedded)
mimetype('.avi', 'video', 'x-msvideo'). % AVI: Audio Video Interleave
mimetype('.bin', 'application', 'octet-stream'). % Any kind of binary data
mimetype('.bz', 'application', 'x-bzip'). % BZip archive
mimetype('.bz2', 'application', 'x-bzip2'). % BZip2 archive
mimetype('.csh', 'application', 'x-csh'). % C-Shell script
mimetype('.css', 'text', 'css'). % Cascading Style Sheets (CSS)
mimetype('.csv', 'text', 'csv'). % Comma-separated values (CSV)
mimetype('.doc', 'application', 'msword'). % Microsoft Word
mimetype('.gif', 'image', 'gif'). % Graphics Interchange Format (GIF)
mimetype('.htm', 'text', 'html'). % HyperText Markup Language (HTML)
mimetype('.html', 'text', 'html'). % HyperText Markup Language (HTML)
mimetype('.ico', 'image', 'x-icon'). % Icon format
mimetype('.jar', 'application', 'java-archive'). % Java Archive (JAR)
mimetype('.jpeg', 'image', 'jpeg'). % JPEG images
mimetype('.jpg', 'image', 'jpeg'). % JPEG images
mimetype('.js', 'application', 'javascript'). % JavaScript (ECMAScript)
mimetype('.json', 'application', 'json'). % JSON format
mimetype('.mid', 'audio', 'midi'). % Musical Instrument Digital Interface (MIDI)
mimetype('.midi', 'audio', 'midi'). % Musical Instrument Digital Interface (MIDI)
mimetype('.mpeg', 'video', 'mpeg'). % MPEG Video
mimetype('.mpkg', 'application', 'vnd.apple.installer+xml'). % Apple Installer Package
mimetype('.odp', 'application', 'vnd.oasis.opendocument.presentation'). % OpenDocuemnt presentation document
mimetype('.ods', 'application', 'vnd.oasis.opendocument.spreadsheet'). % OpenDocuemnt spreadsheet document
mimetype('.odt', 'application', 'vnd.oasis.opendocument.text'). % OpenDocument text document
mimetype('.oga', 'audio', 'ogg'). % OGG audio
mimetype('.ogv', 'video', 'ogg'). % OGG video
mimetype('.ogx', 'application', 'ogg'). % OGG
mimetype('.otf', 'font', 'otf'). % OpenType font
mimetype('.png', 'image', 'png'). % Portable Network Graphics
mimetype('.pdf', 'application', 'pdf'). % Adobe Portable Document Format (PDF)
mimetype('.ppt', 'application', 'vnd.ms-powerpoint'). % Microsoft PowerPoint
mimetype('.rar', 'application', 'x-rar-compressed'). % RAR archive
mimetype('.rtf', 'application', 'rtf'). % Rich Text Format (RTF)
mimetype('.sh', 'application', 'x-sh'). % Bourne shell script
mimetype('.svg', 'image', 'svg+xml'). % Scalable Vector Graphics (SVG)
mimetype('.swf', 'application', 'x-shockwave-flash'). % Small web format (SWF) or Adobe Flash document
mimetype('.tar', 'application', 'x-tar'). % Tape Archive (TAR)
mimetype('.tif', 'image', 'tiff'). % Tagged Image File Format (TIFF)
mimetype('.tiff', 'image', 'tiff'). % Tagged Image File Format (TIFF)
mimetype('.ts', 'application', 'typescript'). % Typescript file
mimetype('.ttf', 'font', 'ttf'). % TrueType Font
mimetype('.wav', 'audio', 'x-wav'). % Waveform Audio Format
mimetype('.weba', 'audio', 'webm'). % WEBM audio
mimetype('.webm', 'video', 'webm'). % WEBM video
mimetype('.webp', 'image', 'webp'). % WEBP image
mimetype('.woff', 'font', 'woff'). % Web Open Font Format (WOFF)
mimetype('.woff2', 'font', 'woff2'). % Web Open Font Format (WOFF)
mimetype('.xhtml', 'application', 'xhtml+xml'). % XHTML
mimetype('.xml', 'application', 'xml'). % XML
mimetype('.zip', 'application', 'zip'). % ZIP archive
mimetype('.7z', 'application', 'x-7z-compressed'). % 7-zip archive
