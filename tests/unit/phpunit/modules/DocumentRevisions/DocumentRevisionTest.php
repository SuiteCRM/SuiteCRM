<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class DocumentRevisionTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testDocumentRevision(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $documentRevision = BeanFactory::newBean('DocumentRevisions');
        self::assertInstanceOf('DocumentRevision', $documentRevision);
        self::assertInstanceOf('SugarBean', $documentRevision);

        self::assertEquals('DocumentRevisions', $documentRevision->module_dir);
        self::assertEquals('DocumentRevision', $documentRevision->object_name);
        self::assertEquals('document_revisions', $documentRevision->table_name);
        self::assertEquals(true, $documentRevision->new_schema);
    }

    public function testSaveAndRetrieve(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        $documentRevision->document_id = '1';
        $documentRevision->doc_id = '1';
        $documentRevision->doc_type = 'text';
        $documentRevision->filename = 'test';
        $documentRevision->file_ext = 'ext';

        $documentRevision->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($documentRevision->id));
        self::assertEquals(36, strlen((string) $documentRevision->id));

        //test document retrieve method
        $docRev = $documentRevision->retrieve($documentRevision->id);
        self::assertEquals('1', $docRev->document_id);
        self::assertEquals('1', $docRev->doc_id);
        self::assertEquals('text', $docRev->doc_type);
        self::assertEquals('test', $docRev->filename);
        self::assertEquals('ext', $docRev->file_ext);

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $docRev->mark_deleted($docRev->id);
        $result = $docRev->retrieve($docRev->id);
        self::assertEquals(null, $result);
    }

    public function testget_summary_text(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        //test without setting name
        self::assertEquals(null, $documentRevision->get_summary_text());

        //test with name set
        $documentRevision->filename = 'test';
        self::assertEquals('test', $documentRevision->get_summary_text());
    }

    public function testis_authenticated(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        //test wihout setting attributes
        self::assertEquals(null, $documentRevision->is_authenticated());

        //test with attributes preset
        $documentRevision->authenticated = true;
        self::assertEquals(true, $documentRevision->is_authenticated());
    }

    public function testfill_in_additional_list_fields(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $documentRevision->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $documentRevision->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetDocumentRevisionNameForDisplay(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        //test wihout setting attributes
        $result = $documentRevision->getDocumentRevisionNameForDisplay();
        self::assertEquals('.', $result);

        //test with attributes preset
        $documentRevision->filename = 'test.ext';
        $documentRevision->revision = 1;
        $result = $documentRevision->getDocumentRevisionNameForDisplay();
        self::assertEquals('-Revision_1.ext', $result);
    }

    public function testfill_document_name_revision(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $documentRevision->fill_document_name_revision('dummy_id');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testlist_view_parse_additional_sections(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');

        $xTemplateSection = null;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $ss = new Sugar_Smarty();
            $documentRevision->list_view_parse_additional_sections($ss, $xTemplateSection);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $result = BeanFactory::newBean('DocumentRevisions')->get_list_view_data();
        self::assertEquals(array('DELETED' => 0), $result);
    }

    public function testget_document_revision_name(): void
    {
        $result = BeanFactory::newBean('DocumentRevisions')->get_document_revision_name(1);
        self::assertEquals(null, $result);
    }

    public function testget_document_revisions(): void
    {
        $results = BeanFactory::newBean('DocumentRevisions')->get_document_revisions(1);
        self::assertIsArray($results);
    }

    public function testbean_implements(): void
    {
        $documentRevision = BeanFactory::newBean('DocumentRevisions');
        self::assertEquals(false, $documentRevision->bean_implements('')); //test with blank value
        self::assertEquals(false, $documentRevision->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $documentRevision->bean_implements('FILE')); //test with valid value
    }
}
