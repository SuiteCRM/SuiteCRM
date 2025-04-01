<?php
namespace Api\V8\BeanDecorator;

use \SugarBean;
use \Person;

#[\AllowDynamicProperties]
class BeanManager
{
    public const DEFAULT_OFFSET = 0;
    public const DEFAULT_LIMIT = -1;
    public const DEFAULT_ALL_RECORDS = -99;

    /**
     * @var \DBManager
     */
    protected $db;

    /**
     * @var array
     */
    protected $beanAliases;

    /**
     * @param \DBManager $db
     * @param array $beanAliases
     */
    public function __construct(\DBManager $db, array $beanAliases)
    {
        $this->db = $db;
        $this->beanAliases = $beanAliases;
    }

    /**
     * @param string $module
     *
     * @return \SugarBean
     * @throws \InvalidArgumentException When the module is invalid.
     */
    public function newBeanSafe($module)
    {
        if (array_key_exists($module, $this->beanAliases)) {
            $module = $this->beanAliases[$module];
        }

        $bean = \BeanFactory::newBean($module);

        if (!$bean instanceof \SugarBean) {
            throw new \InvalidArgumentException(sprintf('Module %s does not exist', $module));
        }

        return $bean;
    }

    /**
     * @param string $module
     * @param string|null $id
     * @param array $params
     * @param boolean $deleted
     *
     * @return \SugarBean|boolean
     */
    public function getBean($module, $id = null, array $params = [], $deleted = true)
    {
        return \BeanFactory::getBean($module, $id, $params, $deleted);
    }

    /**
     * @param string $module
     * @param string $id
     * @param array $params
     * @param boolean $deleted
     *
     * @return \SugarBean
     * @throws \DomainException When bean id is empty or bean is not found by name.
     * @throws \InvalidArgumentException When bean is not found with the given id.
     */
    public function getBeanSafe(
        $module,
        $id,
        array $params = [],
        $deleted = true
    ) {
        if (empty($id)) {
            throw new \DomainException('Module id is empty when trying to get ' . $module);
        }

        $objectName = \BeanFactory::getObjectName($module);
        if (!$objectName && array_key_exists($module, $this->beanAliases)) {
            $objectName = \BeanFactory::getObjectName($this->beanAliases[$module]);
            $module = $this->beanAliases[$module];
        }

        if (!$objectName) {
            throw new \DomainException(sprintf('Module with name %s is not found', $module));
        }

        $bean = $this->getBean($module, $id, $params, $deleted);
        if ($bean === false) {
            throw new \InvalidArgumentException(
                sprintf('%s module with id %s is not found', $module, $id)
            );
        }

        return $bean;
    }

    /**
     * @param string $module
     *
     * @return BeanListRequest
     */
    public function getList($module)
    {
        return new BeanListRequest($this->newBeanSafe($module));
    }

    /**
     * @param \SugarBean $sourceBean
     * @param \SugarBean $relatedBean
     * @param string $relationship
     *
     * @throws \RuntimeException If relationship cannot be loaded or created between beans.
     */
    public function createRelationshipSafe(\SugarBean $sourceBean, \SugarBean $relatedBean, $relationship)
    {
        if (!$sourceBean->load_relationship($relationship)) {
            throw new \RuntimeException(
                sprintf('Cannot load relationship %s for module %s', $relationship, $sourceBean->getObjectName())
            );
        }

        $result = $sourceBean->{$relationship}->add($relatedBean);

        if (!$result) {
            throw new \RuntimeException(
                sprintf(
                    'Cannot create relationship %s between module %s and %s',
                    $relationship,
                    $sourceBean->getObjectName(),
                    $relatedBean->getObjectName()
                )
            );
        }
    }

    /**
     * @param \SugarBean $sourceBean
     * @param \SugarBean $relatedBean
     * @param string $relationship
     *
     * @throws \RuntimeException If relationship cannot be loaded or deleted between beans.
     */
    public function deleteRelationshipSafe(\SugarBean $sourceBean, \SugarBean $relatedBean, $relationship)
    {
        if (!$sourceBean->load_relationship($relationship)) {
            throw new \RuntimeException(
                sprintf('Cannot load relationship %s for module %s', $relationship, $sourceBean->getObjectName())
            );
        }

        $result = $sourceBean->{$relationship}->delete($sourceBean->id, $relatedBean->id);

        if (!$result) {
            throw new \RuntimeException(
                sprintf(
                    'Cannot delete relationship %s between module %s and %s',
                    $relationship,
                    $sourceBean->getObjectName(),
                    $relatedBean->getObjectName()
                )
            );
        }
    }

    /**
     * @param \SugarBean $sourceBean
     * @param \SugarBean $relatedBean
     *
     * @return string
     * @throws \DomainException In case link field is not found.
     */
    public function getLinkedFieldName(\SugarBean $sourceBean, \SugarBean $relatedBean)
    {
        $linkedFields = $sourceBean->get_linked_fields();
        $relationship = \Relationship::retrieve_by_modules(
            $sourceBean->module_name,
            $relatedBean->module_name,
            $sourceBean->db
        );

        $linkFieldName = '';
        foreach ($linkedFields as $linkedField) {
            if ($linkedField['relationship'] === $relationship) {
                $linkFieldName = $linkedField['name'];
            }
        }

        if (!$linkFieldName) {
            throw new \DomainException(
                sprintf(
                    'Link field has not found in %s to determine relationship for %s',
                    $sourceBean->getObjectName(),
                    $relatedBean->getObjectName()
                )
            );
        }

        return $linkFieldName;
    }

    /**
     * @param \SugarBean $sourceBean
     * @param string $linkFieldName
     * @return SugarBean
     */
    public function getLinkedFieldBean(\SugarBean $sourceBean, $linkFieldName)
    {
        if (!$sourceBean->load_relationship($linkFieldName)) {
            throw new \RuntimeException(
                sprintf('Cannot load relationship %s for %s module', $linkFieldName, $sourceBean->getObjectName())
            );
        }

        $linkFieldModule = $sourceBean->$linkFieldName->getRelatedModuleName();
        $linkFieldBean = $this->getBean($linkFieldModule);

        if (!$linkFieldBean) {
            throw new \DomainException(
                sprintf(
                    'Link field has not found in %s to determine relationship for %s',
                    $sourceBean->getObjectName(),
                    $linkFieldName
                )
            );
        }

        return $linkFieldBean;
    }

    /**
     * @param string $module
     * @param string $where
     *
     * @return integer
     */
    public function countRecords($module, $where)
    {
        $table_name = $this->newBeanSafe($module)->getTableName();
        $table_name_cstm = $this->newBeanSafe($module)->get_custom_table_name();

        $join = '';
        if ($this->db->tableExists($table_name_cstm)) {
            $join = sprintf(
                'LEFT JOIN %s on (%s.id = %s.id_c)',
                $table_name_cstm,
                $table_name,
                $table_name_cstm,
            );
        }

        $rowCount = $this->db->fetchRow(
            $this->db->query(
                sprintf(
                    "SELECT COUNT(*) AS cnt FROM %s %s %s",
                    $table_name,
                    $join,
                    $where === '' ? '' : 'WHERE ' .  $where
                )
            )
        )["cnt"];

        return (int)$rowCount;
    }

    /**
     * @param SugarBean $bean
     * @return array
     */
    public function getDefaultFields(SugarBean $bean)
    {
        return array_column($bean->field_defs, 'name');
    }

    /**
     * @param SugarBean $bean
     * @param array $fields
     * @return array
     */
    public function filterAcceptanceFields(SugarBean $bean, array $fields)
    {
        if (!$bean instanceof Person) {
            return $fields;
        }

        return array_filter(
            $fields,
            static function ($field) use ($bean) {
                return (
                    in_array($field, array_column($bean->field_defs, 'name'), true)
                    && (
                        empty($bean->field_defs[$field]['link_type'])
                        || $bean->field_defs[$field]['link_type'] !== 'relationship_info'
                    )
                );
            }
        );
    }
}
