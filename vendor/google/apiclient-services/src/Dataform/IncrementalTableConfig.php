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

namespace Google\Service\Dataform;

class IncrementalTableConfig extends \Google\Collection
{
  protected $collection_key = 'uniqueKeyParts';
  /**
   * @var string[]
   */
  public $incrementalPostOperations;
  /**
   * @var string[]
   */
  public $incrementalPreOperations;
  /**
   * @var string
   */
  public $incrementalSelectQuery;
  /**
   * @var bool
   */
  public $refreshDisabled;
  /**
   * @var string[]
   */
  public $uniqueKeyParts;
  /**
   * @var string
   */
  public $updatePartitionFilter;

  /**
   * @param string[]
   */
  public function setIncrementalPostOperations($incrementalPostOperations)
  {
    $this->incrementalPostOperations = $incrementalPostOperations;
  }
  /**
   * @return string[]
   */
  public function getIncrementalPostOperations()
  {
    return $this->incrementalPostOperations;
  }
  /**
   * @param string[]
   */
  public function setIncrementalPreOperations($incrementalPreOperations)
  {
    $this->incrementalPreOperations = $incrementalPreOperations;
  }
  /**
   * @return string[]
   */
  public function getIncrementalPreOperations()
  {
    return $this->incrementalPreOperations;
  }
  /**
   * @param string
   */
  public function setIncrementalSelectQuery($incrementalSelectQuery)
  {
    $this->incrementalSelectQuery = $incrementalSelectQuery;
  }
  /**
   * @return string
   */
  public function getIncrementalSelectQuery()
  {
    return $this->incrementalSelectQuery;
  }
  /**
   * @param bool
   */
  public function setRefreshDisabled($refreshDisabled)
  {
    $this->refreshDisabled = $refreshDisabled;
  }
  /**
   * @return bool
   */
  public function getRefreshDisabled()
  {
    return $this->refreshDisabled;
  }
  /**
   * @param string[]
   */
  public function setUniqueKeyParts($uniqueKeyParts)
  {
    $this->uniqueKeyParts = $uniqueKeyParts;
  }
  /**
   * @return string[]
   */
  public function getUniqueKeyParts()
  {
    return $this->uniqueKeyParts;
  }
  /**
   * @param string
   */
  public function setUpdatePartitionFilter($updatePartitionFilter)
  {
    $this->updatePartitionFilter = $updatePartitionFilter;
  }
  /**
   * @return string
   */
  public function getUpdatePartitionFilter()
  {
    return $this->updatePartitionFilter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IncrementalTableConfig::class, 'Google_Service_Dataform_IncrementalTableConfig');
