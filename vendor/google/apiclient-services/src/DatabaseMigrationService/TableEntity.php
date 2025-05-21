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

class TableEntity extends \Google\Collection
{
  protected $collection_key = 'triggers';
  protected $columnsType = ColumnEntity::class;
  protected $columnsDataType = 'array';
  /**
   * @var string
   */
  public $comment;
  protected $constraintsType = ConstraintEntity::class;
  protected $constraintsDataType = 'array';
  /**
   * @var array[]
   */
  public $customFeatures;
  protected $indicesType = IndexEntity::class;
  protected $indicesDataType = 'array';
  protected $triggersType = TriggerEntity::class;
  protected $triggersDataType = 'array';

  /**
   * @param ColumnEntity[]
   */
  public function setColumns($columns)
  {
    $this->columns = $columns;
  }
  /**
   * @return ColumnEntity[]
   */
  public function getColumns()
  {
    return $this->columns;
  }
  /**
   * @param string
   */
  public function setComment($comment)
  {
    $this->comment = $comment;
  }
  /**
   * @return string
   */
  public function getComment()
  {
    return $this->comment;
  }
  /**
   * @param ConstraintEntity[]
   */
  public function setConstraints($constraints)
  {
    $this->constraints = $constraints;
  }
  /**
   * @return ConstraintEntity[]
   */
  public function getConstraints()
  {
    return $this->constraints;
  }
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
   * @param IndexEntity[]
   */
  public function setIndices($indices)
  {
    $this->indices = $indices;
  }
  /**
   * @return IndexEntity[]
   */
  public function getIndices()
  {
    return $this->indices;
  }
  /**
   * @param TriggerEntity[]
   */
  public function setTriggers($triggers)
  {
    $this->triggers = $triggers;
  }
  /**
   * @return TriggerEntity[]
   */
  public function getTriggers()
  {
    return $this->triggers;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TableEntity::class, 'Google_Service_DatabaseMigrationService_TableEntity');
