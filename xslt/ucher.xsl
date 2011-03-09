<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
  <xsl:output method="xml" encoding="utf-8"/>
  
  <xsl:template  match="/">
    <УниверсальныйСправочник Группа="Учреждения" Код="Учреждения" Наименование="Учреждения" Иерархический="Да" РежимВыбораЗаписей="НеГрупповые">
      <Описание>
      </Описание>
      <ПериодДействия Начало="" Конец="31.12.9999 0:00:00" />
      <ПривязкаУчреждений />
      <Атрибуты>
        <Атрибут1 Номер="1" Код="ПорядковыйНомер" Наименование="ПорядковыйНомер" ТипЗначения="Число" />
        <Атрибут2 Номер="2" Код="ТерриториальнаяПринадлежность" Наименование="ТерриториальнаяПринадлежность" ТипЗначения="СсылкаНаСправочник{Учреждения.ТерриториальнаяПринадлежность}" />
      </Атрибуты>
      <Записи>
        <xsl:apply-templates mode="root"/>
      </Записи>
    </УниверсальныйСправочник>
  </xsl:template>
  
  <xsl:template mode="root" match="СериализуемыйСправочникУчреждений">
    <xsl:for-each select="СправочникУчреждений/СериализуемоеУчреждение">
      <xsl:call-template name="t2"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="t2">
    <xsl:if test="Код=&quot;ЮгТюмОбласти&quot;">
      <xsl:call-template name="t1"/>
    </xsl:if>
    <xsl:if test="Код=&quot;ТестовыеУчреждения&quot;">
      <xsl:call-template name="t1"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="t1" match="СериализуемоеУчреждение">
    <xsl:element name="Запись">
      <xsl:attribute name="Код">
        <xsl:value-of select="Код"/>
      </xsl:attribute>
      <xsl:attribute name="Наименование">
        <xsl:value-of select="Наименование"/>
      </xsl:attribute>
      <xsl:if test="Атрибут1">
        <xsl:attribute name="Атрибут1">
          <xsl:value-of select="Атрибут1"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="Атрибут2">
        <xsl:attribute name="Атрибут2">
          <xsl:value-of select="Атрибут2"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:for-each select="ПодчиненныеУчреждения/СериализуемоеУчреждение">
        <xsl:call-template name="t1"/>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>

