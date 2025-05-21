<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\DatabaseMigrationService;

class ConstraintEntity extends \Google\Collection
{
  protected $collection_key = 'tableColumns';
  /**
   * @var array[]
   */
  public $customFeatures;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $referenceColumns;
  /**
   * @var string
   */
  public $referenceTable;
  /**
   * @var string[]
   */
  public $tableColumns;
  /**
   * @var string
   */
  public $tableName;
  /**
   * @var string
   */
  public $type;

  /**
   * @param array[]
   */
  public function setCustomFeatures($customFeatures)
  {
    $this->customFeatures = $customFeatures;
  }
  /**
   * @return array[]
   */
  public function getCustomFeatures()
  {
    return $this->customFeatures;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string[]
   */
  public function setReferenceColumns($referenceColumns)
  {
    $this->referenceColumns = $referenceColumns;
  }
  /**
   * @return string[]
   */
  public function getReferenceColumns()
  {
    return $this->referenceColumns;
  }
  /**
   * @param string
   */
  public function setReferenceTable($referenceTable)
  {
    $this->referenceTable = $referenceTable;
  }
  /**
   * @return string
   */
  public function getReferenceTable()
  {
    return $this->referenceTable;
  }
  /**
   * @param string[]
   */
  public function setTableColumns($tableColumns)
  {
    $this->tableColumns = $tableColumns;
  }
  /**
   * @return string[]
   */
  public function getTableColumns()
  {
    return $this->tableColumns;
  }
  /**
   * @param string
   */
  public function setTableName($tableName)
  {
    $this->tableName = $tableName;
  }
  /**
   * @return string
   */
  public function getTableName()
  {
    return $this->tableName;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConstraintEntity::class, 'Google_Service_DatabaseMigrationService_ConstraintEntity');
