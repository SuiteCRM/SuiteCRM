<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class CurrencyTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testCurrency(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $currency = BeanFactory::newBean('Currencies');
        self::assertInstanceOf('Currency', $currency);
        self::assertInstanceOf('SugarBean', $currency);

        self::assertEquals('Currencies', $currency->module_dir);
        self::assertEquals('Currency', $currency->object_name);
        self::assertEquals('currencies', $currency->table_name);
        self::assertEquals(true, $currency->disable_num_format);
        self::assertEquals(true, $currency->new_schema);
    }

    public function testconvertToDollar(): void
    {
        $currency = BeanFactory::newBean('Currencies');

        //test without setting attributes
        self::assertEquals(0, $currency->convertToDollar(100, 2));

        //test with required attributes set
        $currency->conversion_rate = 1.6;
        self::assertEquals(62.5, $currency->convertToDollar(100, 2));
    }

    public function testconvertFromDollar(): void
    {
        $currency = BeanFactory::newBean('Currencies');

        //test without setting attributes
        self::assertEquals(0, $currency->convertFromDollar(100, 2));

        //test with required attributes set
        $currency->conversion_rate = 1.6;
        self::assertEquals(160, $currency->convertFromDollar(100, 2));
    }

    public function testgetDefaultCurrencyName(): void
    {
        $currency = BeanFactory::newBean('Currencies');
        self::assertEquals('US Dollars', $currency->getDefaultCurrencyName());
    }

    public function testgetDefaultCurrencySymbol(): void
    {
        $currency = BeanFactory::newBean('Currencies');
        self::assertEquals('$', $currency->getDefaultCurrencySymbol());
    }

    public function testgetDefaultISO4217(): void
    {
        $currency = BeanFactory::newBean('Currencies');
        self::assertEquals('USD', $currency->getDefaultISO4217());
    }

    public function testretrieveIDBySymbol(): void
    {
        $currency = BeanFactory::newBean('Currencies');
        self::assertEquals('', $currency->retrieveIDBySymbol(''));
        self::assertEquals('', $currency->retrieveIDBySymbol('\$'));
    }

    public function testlist_view_parse_additional_sections(): void
    {
        global $isMerge;

        $currency = BeanFactory::newBean('Currencies');

        //test without setting attributes
        $ss = new Sugar_Smarty();
        $result = $currency->list_view_parse_additional_sections($ss);
        self::assertEquals(null, isset($result->tpl_vars['PREROW']->value) ? $result->tpl_vars['PREROW']->value : null);

        //test with required attributes set
        $isMerge = true;
        $ss = new Sugar_Smarty();
        $result = $currency->list_view_parse_additional_sections($ss);
        self::assertEquals('<input name="mergecur[]" type="checkbox" value="">', $result->tpl_vars['PREROW']->value);
    }

    public function testretrieve_id_by_name(): void
    {
        $currency = BeanFactory::newBean('Currencies');
        self::assertEquals('', $currency->retrieve_id_by_name(''));
        self::assertEquals('', $currency->retrieve_id_by_name('US Dollars'));
    }

    public function testretrieve(): void
    {
        $currency = BeanFactory::newBean('Currencies');

        //execute the method and verify that it returns expected results
        $currency->retrieve();

        self::assertEquals('US Dollars', $currency->name);
        self::assertEquals('$', $currency->symbol);
        self::assertEquals('-99', $currency->id);
        self::assertEquals(1, $currency->conversion_rate);
        self::assertEquals('USD', $currency->iso4217);
        self::assertEquals(0, $currency->deleted);
        self::assertEquals('Active', $currency->status);
        self::assertEquals('<!--', $currency->hide);
        self::assertEquals('-->', $currency->unhide);
    }

    public function testgetPdfCurrencySymbol(): void
    {
        $currency = BeanFactory::newBean('Currencies');

        //test without setting attributes
        self::assertEquals('', $currency->getPdfCurrencySymbol());

        //test with required attributes set
        $currency->symbol = '�';
        self::assertEquals('�', $currency->getPdfCurrencySymbol());
    }

    public function testget_list_view_data(): void
    {
        $currency = BeanFactory::newBean('Currencies');

        //execute the method and verify that it retunrs expected results
        $expected = array(
            'CONVERSION_RATE' => '0.0000000000',
            'HIDE' => '',
            'UNHIDE' => '',
        );

        $actual = $currency->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testsave(): void
    {
        $currency = BeanFactory::newBean('Currencies');
        $currency->name = 'Rand';
        $currency->iso4217 = 'R';
        $currency->symbol = 'SA Rand';
        $currency->conversion_rate = '2';

        $currency->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($currency->id));
        self::assertEquals(36, strlen((string) $currency->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $currency->mark_deleted($currency->id);
        $result = $currency->retrieve($currency->id);
        self::assertEquals(-99, $result->id);
    }

    public function testcurrency_format_number(): void
    {
        self::assertEquals('$100.00', currency_format_number(100));
        self::assertEquals('$100.0', currency_format_number(100, array('round' => 1, 'decimals' => 1)));
    }

    public function testformat_number(): void
    {
        self::assertEquals('100.00', format_number(100));
        self::assertEquals('100.1', format_number(100.09, 1, 1));
        self::assertEquals('$100.1', format_number(100.09, 1, 1, array('convert' => 1, 'currency_symbol' => 'R')));
    }

    public function testformat_place_symbol(): void
    {
        self::assertEquals('R&nbsp;100', format_place_symbol(100, 'R', true));
        self::assertEquals('R100', format_place_symbol(100, 'R', false));
        self::assertEquals('100', format_place_symbol(100, '', false));
    }

    public function testunformat_number(): void
    {
        self::assertEquals('100', unformat_number('$100'));
        self::assertEquals('100', unformat_number(100));
    }

    public function testformat_money(): void
    {
        self::assertEquals('100.00', format_money('100'));
        self::assertEquals('100.00', format_money('100', false));
    }

    public function testget_number_separators(): void
    {
        self::assertEquals([',', '.'], get_number_separators());
        self::assertEquals([',', '.'], get_number_separators(false));
    }

    public function testget_number_seperators(): void
    {
        self::assertEquals(null, get_number_seperators(false));
    }

    public function testtoString(): void
    {
        $expected = "\$m_currency_round= \n\$m_currency_decimal= \n\$m_currency_symbol= \n\$m_currency_iso= \n\$m_currency_name= \n";
        self::assertSame($expected, toString(false));
    }

    public function testgetCurrencyDropDown(): void
    {
        self::markTestIncomplete('#Warning: Strings contain different line endings!');
        //test with view = Default / DetailView
        self::assertEquals('US Dollars', getCurrencyDropDown(null));

        //test with view = EditView
        $expected = "<select name=\"currency_id\" id=\"currency_id_select\" onchange=\"CurrencyConvertAll(this.form);\"><option value=\"-99\" selected>US Dollars : $</select><script>var ConversionRates = new Array(); \nvar CurrencySymbols = new Array(); \nvar lastRate = \"1\"; ConversionRates['-99'] = '1';\n CurrencySymbols['-99'] = '$';\nvar currencyFields = [];\n					function get_rate(id){\n						return ConversionRates[id];\n					}\n					function ConvertToDollar(amount, rate){\n						return amount / rate;\n					}\n					function ConvertFromDollar(amount, rate){\n						return amount * rate;\n					}\n					function ConvertRate(id,fields){\n							for(var i = 0; i < fields.length; i++){\n								fields[i].value = toDecimal(ConvertFromDollar(toDecimal(ConvertToDollar(toDecimal(fields[i].value), lastRate)), ConversionRates[id]));\n							}\n							lastRate = ConversionRates[id];\n						}\n					function ConvertRateSingle(id,field){\n						var temp = field.innerHTML.substring(1, field.innerHTML.length);\n						unformattedNumber = unformatNumber(temp, num_grp_sep, dec_sep);\n						\n						field.innerHTML = CurrencySymbols[id] + formatNumber(toDecimal(ConvertFromDollar(ConvertToDollar(unformattedNumber, lastRate), ConversionRates[id])), num_grp_sep, dec_sep, 2, 2);\n						lastRate = ConversionRates[id];\n					}\n					function CurrencyConvertAll(form){\n                        try {\n                        var id = form.currency_id.options[form.currency_id.selectedIndex].value;\n						var fields = new Array();\n						\n						for(i in currencyFields){\n							var field = currencyFields[i];\n							if(typeof(form[field]) != 'undefined'){\n								form[field].value = unformatNumber(form[field].value, num_grp_sep, dec_sep);\n								fields.push(form[field]);\n							}\n							\n						}\n							\n							ConvertRate(id, fields);\n						for(i in fields){\n							fields[i].value = formatNumber(fields[i].value, num_grp_sep, dec_sep);\n\n						}\n							\n						} catch (err) {\n                            // Do nothing, if we can't find the currency_id field we will just not attempt to convert currencies\n                            // This typically only happens in lead conversion and quick creates, where the currency_id field may be named somethnig else or hidden deep inside a sub-form.\n                        }\n						\n					}\n				</script>";
        $actual = getCurrencyDropDown(null, 'currency_id', '', 'EditView');
        self::assertSame($expected, $actual);
    }

    public function testgetCurrencyNameDropDown(): void
    {
        //test with view = Default / DetailView
        self::assertEquals('US Dollars', getCurrencyNameDropDown(null));

        //test with view = EditView
        $expected = $expected = "<select name=\"currency_name\" id=\"currency_name\" />\n<OPTION value='US Dollars'>US Dollars</OPTION></select>";
        $actual = getCurrencyNameDropDown(null, 'currency_name', '', 'EditView');
        self::assertSame($expected, $actual);
    }

    public function testgetCurrencySymbolDropDown(): void
    {
        //test with view = Default / DetailView
        self::assertEquals('US Dollars', getCurrencySymbolDropDown(null));

        //test with view = EditView
        $expected = $expected = "<select name=\"currency_name\" id=\"currency_name\" />\n<OPTION value='\$'>\$</OPTION></select>";
        $actual = getCurrencySymbolDropDown(null, 'currency_name', '', 'EditView');
        self::assertSame($expected, $actual);
    }
}
