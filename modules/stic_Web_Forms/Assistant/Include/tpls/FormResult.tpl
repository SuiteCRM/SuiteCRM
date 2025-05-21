{*
********************************************************************************
 Generated form Template
********************************************************************************
*}
<html {$MAP.LANGHEADER}>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">

		<!-- css -->
		{foreach from=$MAP.FORM.CSS item=CSS}
			<link rel="stylesheet" type="text/css" media="all" href="{$CSS}">
		{/foreach}

		{literal}
			<style>
				body {background:transparent; padding: 0px 0px 0px 50px; font-family: Helvetica, sans-serif, Verdana, Arial;}
				h3 {font-size: 1.2em}
				input, textarea, select {width: 200px;}
				.document{width: 300px;}
				.tableForm {width: 100%; text-align: left; padding: 10px 6px 12px 10px;}
				.tableForm tr td {padding: 0px 0px 15px 0px;}
				.column_25 {width: 25%;}
				.required {color: rgb(255, 0, 0);}
				.current-required-field {background-color: yellow;}
				.error_zone {color: red; font-weight: bold; line-height: 30px;}
				.datetimecombo_time_section { display: inline-block; }
				.datetimecombo_time_section select { max-width: 60px; padding-right: 2px; }
			</style>
		{/literal}
		<!-- // css -->
		
		<!-- js scripts -->
		{foreach from=$MAP.FORM.SCRIPTS item=SCRIPT}
			<script type="text/javascript" src="{$SCRIPT}"></script>
		{/foreach}
		{foreach from=$MAP.FORM.SCRIPTS_DEFER item=SCRIPT_DEFER}
			<script type="text/javascript" src="{$SCRIPT_DEFER}" async defer></script>
		{/foreach}
		<!-- // js scripts -->
	</head>

	<body>
		<form action="{$MAP.FORM.URL}" name="{$MAP.FORM.NAME|default:"WebToLeadForm"}" method="POST" id="{$MAP.FORM.ID|default:"WebToLeadForm"}" {if $MAP.FORM.NUM_ATTACHMENT > 0} enctype="multipart/form-data"{/if}>
			{$MAP.HTML}
		</form>		
		<script type="text/javascript">
			{foreach from=$MAP.LANG_VARS key=KEY item=TEXT}
				var {$KEY} = "{$TEXT|escape:'javascript'}";
			{/foreach}
			{$MAP.EMBEDDED_JS}
		</script>
	</body>
</html>
