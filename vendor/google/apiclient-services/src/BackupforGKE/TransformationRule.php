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

namespace Google\Service\BackupforGKE;

class TransformationRule extends \Google\Collection
{
  protected $collection_key = 'fieldActions';
  /**
   * @var string
   */
  public $description;
  protected $fieldActionsType = TransformationRuleAction::class;
  protected $fieldActionsDataType = 'array';
  protected $resourceFilterType = ResourceFilter::class;
  protected $resourceFilterDataType = '';

  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param TransformationRuleAction[]
   */
  public function setFieldActions($fieldActions)
  {
    $this->fieldActions = $fieldActions;
  }
  /**
   * @return TransformationRuleAction[]
   */
  public function getFieldActions()
  {
    return $this->fieldActions;
  }
  /**
   * @param ResourceFilter
   */
  public function setResourceFilter(ResourceFilter $resourceFilter)
  {
    $this->resourceFilter = $resourceFilter;
  }
  /**
   * @return ResourceFilter
   */
  public function getResourceFilter()
  {
    return $this->resourceFilter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TransformationRule::class, 'Google_Service_BackupforGKE_TransformationRule');
