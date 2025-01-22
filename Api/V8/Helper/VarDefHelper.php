<?php
namespace Api\V8\Helper;
#[\AllowDynamicProperties]
class VarDefHelper
{
    /**
     * @param \SugarBean $bean
     *
     * @return array
     */
    public function getAllRelationships(\SugarBean $bean)
    {
        $relations = [];
        $linkedFields = $bean->get_linked_fields();
        foreach ($linkedFields as $relation => $varDef) {
            if (isset($varDef['module']) && $bean->load_relationship($relation)) {
                $relations[$relation] = $varDef['module'];
            }
        }
        return $relations;
    }
}