<?php

//require_once 'modules/AOR_Charts/lib/pChart/pChart.php';
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOR_ChartTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOR_Chart(): void
    {
        self::markTestSkipped('Skipping AOR Charts Tests');
        // Execute the constructor and check for the Object type and  attributes
        $aorChart = BeanFactory::newBean('AOR_Charts');
        self::assertInstanceOf('AOR_Chart', $aorChart);
        self::assertInstanceOf('Basic', $aorChart);
        self::assertInstanceOf('SugarBean', $aorChart);

        self::assertEquals('AOR_Charts', $aorChart->module_dir);
        self::assertEquals('AOR_Chart', $aorChart->object_name);
        self::assertEquals('aor_charts', $aorChart->table_name);
        self::assertEquals(true, $aorChart->new_schema);
        self::assertEquals(true, $aorChart->disable_row_level_security);
        self::assertEquals(true, $aorChart->importable);
    }

    public function testsave_lines(): void
    {
        self::markTestSkipped('Skipping AOR Charts Tests');

        $aorChart = BeanFactory::newBean('AOR_Charts');

        //preset the required data
        $post = array();
        $post['chartid'] = array('test' => '');
        $post['charttitle'] = array('test' => 'test');
        $post['charttype'] = array('test' => 'bar');
        $post['chartx_field'] = array('test' => '1');
        $post['charty_field'] = array('test' => '2');

        $postKey = 'chart';

        $bean = BeanFactory::newBean('AOR_Reports');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $aorChart->save_lines($post, $bean, $postKey);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testbuildChartImageBar(): void
    {
        self::markTestSkipped('Skipping AOR Charts Tests');

        $aorChart = BeanFactory::newBean('AOR_Charts');

        $chartData = new pData();
        $chartData->addPoints(10, 'data');
        $chartData->addPoints(10, 'Labels');

        //execute with recordImageMap true and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $aorChart->buildChartImageBar($chartPicture, true);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        //execute with recordImageMap false and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $aorChart->buildChartImageBar($chartPicture, false);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        unset($chartData);
        unset($chartPicture);
    }

    public function testbuildChartImagePie(): void
    {
        self::markTestSkipped('Skipping AOR Charts Tests');

        $aorChart = BeanFactory::newBean('AOR_Charts');

        $chartData = new pData();
        $chartData->addPoints(10, 'data');
        $chartData->addPoints(10, 'Labels');

        //execute with recordImageMap false  and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $aorChart->buildChartImagePie($chartPicture, $chartData, array(array('x' => 10), array('x' => 20)), 700, 700, 'x', false);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        //execute with recordImageMap true and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $aorChart->buildChartImagePie($chartPicture, $chartData, array(array('x' => 10), array('x' => 20)), 700, 700, 'x', true);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        unset($chartData);
        unset($chartPicture);
    }

    public function testbuildChartImageLine(): void
    {
        self::markTestSkipped('Skipping AOR Charts Tests');

        $aorChart = BeanFactory::newBean('AOR_Charts');

        $chartData = new pData();
        $chartData->addPoints(10, 'data');
        $chartData->addPoints(10, 'Labels');

        //execute with recordImageMap true  and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $aorChart->buildChartImageLine($chartPicture, true);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        //execute with recordImageMap false  and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $aorChart->buildChartImageLine($chartPicture, false);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        unset($chartData);
        unset($chartPicture);
    }

    public function testbuildChartImageRadar(): void
    {
        self::markTestSkipped('Skipping AOR Charts Tests');

        $aorChart = BeanFactory::newBean('AOR_Charts');

        //preset the required objects and properties
        $chartData = new pData();
        $chartData->loadPalette('modules/AOR_Charts/lib/pChart/palettes/navy.color', true);
        $chartData->addPoints(10, 'data');
        $chartData->addPoints('10', 'Labels');
        $chartData->addPoints(120, 'data');
        $chartData->addPoints('120', 'Labels');

        $chartData->setSerieDescription('Months', 'Month');
        $chartData->setAbscissa('Labels');

        $imageWidth = 700;
        $imageHeight = 700;

        //execute with recordImageMap true and verify that the chartPicture is changed
        $chartPicture = new pImage($imageWidth, $imageHeight, $chartData);
        $chartPicture->setGraphArea(60, 60, $imageWidth - 60, $imageHeight - 100);
        $aorChart->buildChartImageRadar($chartPicture, $chartData, true);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        //execute with recordImageMap false and verify that the chartPicture is changed
        $chartPicture = new pImage(700, 700, $chartData);
        $chartPicture->setGraphArea(60, 60, $imageWidth - 60, $imageHeight - 100);
        $aorChart->buildChartImageRadar($chartPicture, $chartData, false);
        self::assertNotEquals($chartPicture, new pImage(700, 700, $chartData));

        unset($chartData);
        unset($chartPicture);
    }

    public function testbuildChartImage(): void
    {
        self::markTestSkipped('Skipping testing chart image');

        global $current_user;

        $current_user->id = '1';

        $aorChart = BeanFactory::newBean('AOR_Charts');

        //preset the required objects and properties

        $aorChart->x_field = 'x';
        $aorChart->y_field = 'y';

        $fields = array();
        $fields['x'] = json_decode('{"label": "x"}');
        $fields['y'] = json_decode('{"label": "y"}');

        $reportData = array(array('xx' => 10, 'yy' => 10), array('xx' => 20, 'yy' => 20));

        //execute the method and verify it returns expected results
        $aorChart->type = 'bar';
        $result = $aorChart->buildChartImage($reportData, $fields);
        self::assertEquals('data:image/png;base64,', $result);

        //execute the method and verify it returns expected results
        $aorChart->type = 'Line';
        $result = $aorChart->buildChartImage($reportData, $fields);
        self::assertEquals('', $result);

        //execute the method and verify it returns expected results
        $aorChart->type = 'pie';
        $result = $aorChart->buildChartImage($reportData, $fields);
        self::assertEquals('data:image/png;base64,', $result);

        //execute the method and verify it returns expected results
        $aorChart->type = 'radar';
        $result = $aorChart->buildChartImage($reportData, $fields, true);
        self::assertEquals('data:image/png;base64,', $result);
    }

    public function testbuildChartHTML(): void
    {
        self::markTestSkipped('Skipping testing chart HTML');
        $aorChart = BeanFactory::newBean('AOR_Charts');

        //preset the required objects and properties
        $aorChart->x_field = 'x';
        $aorChart->y_field = 'y';

        $fields = array();
        $fields['x'] = json_decode('{"label": "x"}');
        $fields['y'] = json_decode('{"label": "y"}');

        $reportData = array(array('xx' => 10, 'yy' => 10), array('xx' => 20, 'yy' => 20));

        $aorChart->type = 'bar';

        //test with type CHART_TYPE_PCHART and verify it returns expected results
        $expected = "<img id='_img' src='data:image/png;base64,'><script>\nSUGAR.util.doWhen(\"typeof addImage != 'undefined'\", function(){\n    addImage('_img','_img_map','index.php?module=AOR_Charts&action=getImageMap&to_pdf=1&imageMapId=0');\n});\n</script>";
        $result = $aorChart->buildChartHTML($reportData, $fields);
        self::assertSame($expected, $result);

        //test with type CHART_TYPE_CHARTJS verify it returns expected results
        $expected = "<h3></h3><canvas id='chart' width='400' height='400'></canvas>        <script>\n        \$(document).ready(function(){\n            SUGAR.util.doWhen(\"typeof Chart != 'undefined'\", function(){\n                var data = {\"labels\":[\"10 [a]\",\"20 [e]\"],\"datasets\":[{\"fillColor\":\"rgba(151,187,205,0.2)\",\"strokeColor\":\"rgba(151,187,205,1)\",\"pointColor\":\"rgba(151,187,205,1)\",\"pointStrokeColor\":\"#fff\",\"pointHighlightFill\":\"#fff\",\"pointHighlightStroke\":\"rgba(151,187,205,1)4\",\"data\":[10,20]}]};\n                var ctx = document.getElementById(\"chart\").getContext(\"2d\");\n                console.log('Creating new chart');\n                var config = [];\n                var chart = new Chart(ctx).Bar(data, config);\n                var legend = chart.generateLegend();\n                $('#chart').after(legend);\n            });\n        });\n        </script>";
        $result = $aorChart->buildChartHTML($reportData, $fields, 0, AOR_Report::CHART_TYPE_CHARTJS);
        self::assertSame($expected, $result);
    }
}
