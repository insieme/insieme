<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:output method="text" indent="yes"/> 
 
	<xsl:template match="/inspire">
		digraph inspire {
			<xsl:apply-templates select="/inspire/*" mode="definition" /> 
			<xsl:apply-templates select="/inspire/*" mode="link" />
		}
	</xsl:template>
 
	<!-- Visit the ROOT node -->
	<xsl:template match="rootNode" mode="definition">
		ROOT [shape=point];
	</xsl:template>
	 
	<!-- Defines a literal node: when a literal node is defined we write the contents of the node 
	     inside the node itself. A problem arises with string literals which already have a "" so
	     we have to guarantee the label f the DOT node doesn't contain multiple "s -->
	<xsl:template match="literal" mode="definition">
		<xsl:variable name="value"> 
			<xsl:choose>
				<!-- this is a string literal, it already contains a trailing "" -->
				<xsl:when test="starts-with(@value,'&quot;')"> <xsl:value-of select="@value"/> </xsl:when>
				<!-- this is not a string literal, we add trailing "" -->
				<xsl:otherwise> "<xsl:value-of select="@value"/>" </xsl:otherwise>
			</xsl:choose>
		 </xsl:variable>
		<xsl:value-of select="@id"/>  [shape=diamond, label=<xsl:value-of select="$value"/>];
	</xsl:template>
	<!-- genType[count(node()/*)=0] -->
	<xsl:template match="genType" mode="definition">
		<xsl:value-of select="@id"/>  [shape=ellipse, color=blue, label=<xsl:value-of select="@familyName"/>];
	</xsl:template>
	
	<!-- this template matches nodes which are different from literals and rootNode. -->
	<xsl:template match="*" mode="definition">
		<xsl:variable name="nodeName"> <xsl:value-of select="name()"/> </xsl:variable>
		<!-- We use a box shape for statements and an ellipse shape for types -->
		<xsl:variable name="nodeShape"> 
			<xsl:choose>
				<xsl:when test="contains($nodeName, 'Type')"> ellipse </xsl:when>
				<xsl:otherwise> box </xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:value-of select="@id"/>  [shape=<xsl:value-of select="$nodeShape"/>, label="<xsl:value-of select="$nodeName"/>"];
	</xsl:template>
 
	<!-- Once the node of the graph are declared we have to start defining the links, the following templates 
	     which are identified with the link 'mode' have the responsability to link nodes according to the 
		 structure of the IR -->
	<xsl:template match="rootNode" mode="link">
		ROOT -> <xsl:value-of select="nodePtr/@ref"/>;
	</xsl:template>
  
	<!--@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	    @@							   	 TYPES 
		@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ -->
	<xsl:template match="genType" mode="link">
		<xsl:variable name = "id"> <xsl:value-of select="@id"/> </xsl:variable> 
		<xsl:for-each select="typeParams/*">
			<xsl:value-of select="$id"/> -> <xsl:value-of select="@ref"/> [label="typeParams_<xsl:value-of select="position()"/>"];
		</xsl:for-each>
		<xsl:for-each select="intTypeParams/*">
			<xsl:value-of select="$id"/> -> <xsl:value-of select="@value"/> [label="intTypeParams_<xsl:value-of select="position()"/>"];
		</xsl:for-each>
	</xsl:template>
		
	<!--@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	    @@							   EXPRESSIONS 
		@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ -->
	<xsl:template match="vectorExpr" mode="link">
		<xsl:value-of select="@id"/> -> <xsl:value-of select="type/typePtr/@ref"/> [label=type];
		
		<!-- don't visit the initexpr for now -->
		<xsl:variable name="id"> <xsl:value-of select="@id"/> </xsl:variable> 
		<!-- <xsl:for-each select="expressions/expression">
			<xsl:value-of select="$id"/> -> <xsl:value-of select="expressionPtr/@ref"/> [label="I#<xsl:value-of select="position()"/>"]; 
			<xsl:variable name="ref"> <xsl:value-of select="expressionPtr/@ref"/> </xsl:variable>  
		</xsl:for-each> -->
		<!-- Handle annotations here-->
	</xsl:template>
  
  	<!-- Generic statement visit -->
	<xsl:template match="*" mode="link">
		<xsl:variable name="id"> <xsl:value-of select="@id"/> </xsl:variable> 
		
		<xsl:for-each select="*/*[@ref]">
			<xsl:variable name="label"> 
				<xsl:choose>
					<!-- this is a string literal, it already contains a trailing "" -->
					<xsl:when test="count(../*) > 1"> 
						<xsl:value-of select="concat(concat(local-name(parent::*), '_'), count(parent::*/preceding-sibling::*) + 1)"/> 
					</xsl:when>
					<!-- this is not a string literal, we add trailing "" -->
					<xsl:otherwise> <xsl:value-of select="local-name(parent::*)"/> </xsl:otherwise>
				</xsl:choose>
			<xsl:value-of select="@id"/> </xsl:variable> 
			<xsl:value-of select="$id"/> -> <xsl:value-of select="@ref"/> [label="<xsl:value-of select="$label"/>"];
		</xsl:for-each>
		
		<xsl:for-each select="*/*/*[@ref]">
			<xsl:variable name="label"> 
				<xsl:choose>
					<!-- this is a string literal, it already contains a trailing "" -->
					<xsl:when test="count(../../*) > 1"> 
						<xsl:value-of select="concat(concat(local-name(parent::*), '_'), count(parent::*/preceding-sibling::*) + 1)"/> 
					</xsl:when>
					<!-- this is not a string literal, we add trailing "" -->
					<xsl:otherwise> <xsl:value-of select="local-name(parent::*)"/> </xsl:otherwise>
				</xsl:choose>
			<xsl:value-of select="@id"/> </xsl:variable> 
			<xsl:value-of select="$id"/> -> <xsl:value-of select="@ref"/> [label="<xsl:value-of select="$label"/>"];
		</xsl:for-each>
	</xsl:template> 
	<!-- annotations -->
	
</xsl:stylesheet>