<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:output method="text" indent="yes"/> 
 
	<xsl:template match="/inspire">
		digraph inspire {
			<xsl:apply-templates select="/inspire/rootNode" mode="link" />
		}
	</xsl:template>
 
	<!-- Once the node of the graph are declared we have to start defining the links, the following templates 
	     which are identified with the link 'mode' have the responsability to link nodes according to the 
		 structure of the IR -->
	<xsl:template match="rootNode" mode="link">
		ROOT -> 0;
		<xsl:variable name="ref"> <xsl:value-of select="nodePtr/@ref"/> </xsl:variable> 
		<xsl:apply-templates select="//*[@id=$ref]">
			<xsl:with-param name="id"> <xsl:value-of select="0" /> </xsl:with-param>
		</xsl:apply-templates>
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
	<xsl:template match="*">
		<xsl:param name="id"/>
		
		<xsl:variable name="nodeName"> <xsl:value-of select="name()"/> </xsl:variable>
		<!-- We use a box shape for statements and an ellipse shape for types -->
		<xsl:variable name="nodeShape"> 
			<xsl:choose>
				<xsl:when test="contains($nodeName, 'Type')"> ellipse </xsl:when>
				<xsl:when test="contains($nodeName, 'literal')"> diamond </xsl:when>
				<xsl:otherwise> box </xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		
		<!-- We use a box shape for statements and an ellipse shape for types -->
		<xsl:variable name="nodeStyle"> 
			<xsl:choose>
				<xsl:when test="contains($nodeName, 'Type')"> solid </xsl:when>
				<xsl:otherwise> filled </xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		
		<xsl:variable name="node_label"> 
			<xsl:choose>
				<xsl:when test="contains($nodeName, 'literal') and starts-with(@value,'&quot;')"> 
					<xsl:value-of select="@value"/> 
				</xsl:when>
				<!-- this is a string literal, it already contains a trailing "" -->
				<xsl:when test="contains($nodeName, 'literal') and not(starts-with(@value,'&quot;'))"> 
					"<xsl:value-of select="@value"/>"  
				</xsl:when>
				<xsl:when test="$nodeName = 'genType'"> 
					"<xsl:value-of select="@familyName"/>"  
				</xsl:when>
				<!-- this is not a string literal, we add trailing "" -->
				<xsl:otherwise>"<xsl:value-of select="$nodeName"/>"</xsl:otherwise>
			</xsl:choose>
		 </xsl:variable>
		 
		<xsl:value-of select="$id"/>  [shape=<xsl:value-of select="$nodeShape"/>, style=<xsl:value-of select="$nodeStyle"/>, label=<xsl:value-of select="$node_label"/>];
		
		<xsl:for-each select="*/*[@ref]">
			<!-- <xsl:if test="local-name(parent::*) != 'type'"> -->
			<xsl:variable name="parent_pos"> <xsl:value-of select="count(../preceding-sibling::*) + 1"/> </xsl:variable> 
			<xsl:variable name="label"> 
				<xsl:choose>
					<!-- this is a string literal, it already contains a trailing "" -->
					<xsl:when test="count(../*) > 1"> 
						<xsl:value-of select="concat(concat(local-name(parent::*), '_'), $parent_pos)"/> 
					</xsl:when>
					<!-- this is not a string literal, we add trailing "" -->
					<xsl:otherwise> <xsl:value-of select="local-name(parent::*)"/> </xsl:otherwise>
				</xsl:choose>
			</xsl:variable> 
			<xsl:variable name="currId"> 
				<xsl:value-of select="concat($id, concat($parent_pos, position()))"/> 
			</xsl:variable> 
			<xsl:variable name="ref"> <xsl:value-of select="@ref"/> </xsl:variable> 
			<xsl:value-of select="$id"/> -> <xsl:value-of select="$currId"/> [label="<xsl:value-of select="$label"/>"];
			<xsl:apply-templates select="//*[@id=$ref]">
				<xsl:with-param name="id"> <xsl:value-of select="$currId"/> </xsl:with-param>
			</xsl:apply-templates>
			<!-- </xsl:if> -->
		</xsl:for-each>
		
		<xsl:for-each select="*/*/*[@ref]">
			<xsl:variable name="parent_pos"> <xsl:value-of select="count(../preceding-sibling::*) + 1"/> </xsl:variable> 
			<xsl:variable name="parent_parent_pos"> <xsl:value-of select="count(../../preceding-sibling::*) + 1"/> </xsl:variable> 
			<xsl:variable name="label"> 
				<xsl:choose>
					<!-- this is a string literal, it already contains a trailing "" -->
					<xsl:when test="count(../../*) > 1"> 
						<xsl:value-of select="concat(concat(local-name(parent::*), '_'), $parent_pos)"/> 
					</xsl:when>
					<!-- this is not a string literal, we add trailing "" -->
					<xsl:otherwise> <xsl:value-of select="local-name(parent::*)"/> </xsl:otherwise>
				</xsl:choose>
			</xsl:variable> 
			<xsl:variable name="currId"> 
				<xsl:value-of select="concat($id, concat($parent_pos, concat($parent_parent_pos, position())))"/> 
			</xsl:variable> 
			<xsl:variable name="ref"> <xsl:value-of select="@ref"/> </xsl:variable> 
			<xsl:value-of select="$id"/> -> <xsl:value-of select="$currId"/> [label="<xsl:value-of select="$label"/>"];
			<xsl:apply-templates select="//*[@id=$ref]">
				<xsl:with-param name="id"> <xsl:value-of select="$currId"/> </xsl:with-param>
			</xsl:apply-templates>
		</xsl:for-each>
		
	</xsl:template> 
	
	
	<!-- annotations -->
	
</xsl:stylesheet>