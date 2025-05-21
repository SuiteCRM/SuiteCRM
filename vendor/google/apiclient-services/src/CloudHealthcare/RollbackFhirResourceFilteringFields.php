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

namespace Google\Service\CloudHealthcare;

class RollbackFhirResourceFilteringFields extends \Google\Collection
{
  protected $collection_key = 'operationIds';
  /**
   * @var string
   */
  public $metadataFilter;
  /**
   * @var string[]
   */
  public $operationIds;

  /**
   * @param string
   */
  public function setMetadataFilter($metadataFilter)
  {
    $this->metadataFilter = $metadataFilter;
  }
  /**
   * @return string
   */
  public function getMetadataFilter()
  {
    return $this->metadataFilter;
  }
  /**
   * @param string[]
   */
  public function setOperationIds($operationIds)
  {
    $this->operationIds = $operationIds;
  }
  /**
   * @return string[]
   */
  public function getOperationIds()
  {
    return $this->operationIds;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RollbackFhirResourceFilteringFields::class, 'Google_Service_CloudHealthcare_RollbackFhirResourceFilteringFields');
