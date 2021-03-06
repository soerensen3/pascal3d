<?xml version="1.0"?>
<?xml-stylesheet type="text/xml" href="#stylesheet"?>
<!DOCTYPE p3dlog [
<!ATTLIST xsl:stylesheet
  id	ID	#REQUIRED>
]>
<p3dlog>
<xsl:stylesheet id="stylesheet"
                version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="xsl:stylesheet"/>
  <xsl:template match="/">  
  <html>
  <head>
     <style>
      h1{
      font-family: Roboto Sans, Arial;
      font-size: 32px;
      }
      td,th{
      font-family: Roboto Sans, Arial;
      white-space: pre-wrap;
      text-align:left;
      vertical-align: top;
      }
      .info{
      background-color: rgb(255, 255, 230);
      }
      .exception{
      background-color: rgb(255, 194, 153);
      }
     </style>  
  </head>
  <body>
  <h1>Logfile</h1>
  <table>
  <tr><th>Time</th><th>Message</th></tr>
  <xsl:apply-templates />
  </table>
  </body>
  </html>
  </xsl:template>
  <xsl:template match="info">
    <tr class="info"><td><xsl:value-of select="@Time"/></td><td><xsl:value-of select="."/></td></tr>
  </xsl:template>
  <xsl:template match="exception">
    <tr class="exception"><td><xsl:value-of select="@Time"/></td><td><xsl:value-of select="."/></td></tr>
  </xsl:template>
</xsl:stylesheet
