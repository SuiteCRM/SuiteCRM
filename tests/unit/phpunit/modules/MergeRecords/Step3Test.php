<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'include/utils/layout_utils.php';

class Step3Test extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var string[]
     */
    private $caseIds = [];

    /**
     * @var array
     */
    private $request;

    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        $current_user = BeanFactory::getBean('Users', '1');

        $this->request = $_REQUEST;
    }

    protected function tearDown(): void
    {
        $_REQUEST = $this->request;

        foreach ($this->caseIds as $id) {
            $this->db->query('DELETE FROM cases WHERE id = ' . $this->db->quoted($id));
        }

        parent::tearDown();
    }

    public function testValuesAreEncodedForTheMergeView(): void
    {
        // With two records, values are truncated to 32 bytes, which splits the multibyte character here.
        $mergedDescription = 'I\'m still "waiting" <b>here</b>é and this text is long enough to be truncated';

        $_REQUEST['merge_module'] = 'Cases';
        $_REQUEST['record'] = $this->createCase('Primary case', 'Primary description');
        $_REQUEST['mass'] = [$this->createCase('O\'Neil case', $mergedDescription)];

        global $mod_strings, $current_language;
        $mod_strings = return_module_language($current_language, 'MergeRecords');

        ob_start();
        include 'modules/MergeRecords/Step3.php';
        $output = ob_get_clean();

        $document = new DOMDocument();
        libxml_use_internal_errors(true);
        $document->loadHTML($output);
        libxml_clear_errors();
        $xpath = new DOMXPath($document);

        $values = [];
        $buttons = [];
        foreach ($xpath->query('//input[@name="copyValue"]') as $button) {
            self::assertSame(1, preg_match('/^copy_value\((.*)\);$/s', $button->getAttribute('onclick'), $matches));
            $data = json_decode($matches[1], true);
            self::assertIsArray($data, 'copy_value() was not given valid JSON');
            $values[$data['field_name']] = $data;
            $buttons[$data['field_name']] = $button;
        }

        self::assertArrayHasKey('description', $values, 'The description field lost its copy button');

        // Values retrieved encoded must not be encoded a second time.
        self::assertSame('O\'Neil case', $values['name']['field_value']);
        // Cases decode their description when retrieved.
        self::assertStringContainsString('I\'m still "waiting"', $values['description']['field_value']);

        $descriptionButton = $buttons['description'];
        self::assertStringContainsString('I\'m still "waiting"', $descriptionButton->getAttribute('title'));
        self::assertSame('button', $descriptionButton->getAttribute('class'));
        // The truncated value next to the button is shown as text, not parsed as markup.
        self::assertStringContainsString('<b>here</b>', $descriptionButton->parentNode->textContent);
        self::assertSame(0, $xpath->query('.//b', $descriptionButton->parentNode)->length);
    }

    /**
     * @param string $name
     * @param string $description
     * @return string
     */
    private function createCase(string $name, string $description): string
    {
        $case = BeanFactory::newBean('Cases');
        $case->name = $name;
        $case->description = $description;
        $case->save();
        $this->caseIds[] = $case->id;

        return $case->id;
    }
}
