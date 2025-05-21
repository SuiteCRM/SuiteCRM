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

namespace Google\Service\SecurityPosture;

class GoogleCloudSecuritypostureV1PolicyRule extends \Google\Model
{
  /**
   * @var bool
   */
  public $allowAll;
  protected $conditionType = Expr::class;
  protected $conditionDataType = '';
  /**
   * @var bool
   */
  public $denyAll;
  /**
   * @var bool
   */
  public $enforce;
  /**
   * @var array[]
   */
  public $parameters;
  protected $resourceTypesType = ResourceTypes::class;
  protected $resourceTypesDataType = '';
  protected $valuesType = GoogleCloudSecuritypostureV1PolicyRuleStringValues::class;
  protected $valuesDataType = '';

  /**
   * @param bool
   */
  public function setAllowAll($allowAll)
  {
    $this->allowAll = $allowAll;
  }
  /**
   * @return bool
   */
  public function getAllowAll()
  {
    return $this->allowAll;
  }
  /**
   * @param Expr
   */
  public function setCondition(Expr $condition)
  {
    $this->condition = $condition;
  }
  /**
   * @return Expr
   */
  public function getCondition()
  {
    return $this->condition;
  }
  /**
   * @param bool
   */
  public function setDenyAll($denyAll)
  {
    $this->denyAll = $denyAll;
  }
  /**
   * @return bool
   */
  public function getDenyAll()
  {
    return $this->denyAll;
  }
  /**
   * @param bool
   */
  public function setEnforce($enforce)
  {
    $this->enforce = $enforce;
  }
  /**
   * @return bool
   */
  public function getEnforce()
  {
    return $this->enforce;
  }
  /**
   * @param array[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return array[]
   */
  public function getParameters()
  {
    return $this->parameters;
  }
  /**
   * @param ResourceTypes
   */
  public function setResourceTypes(ResourceTypes $resourceTypes)
  {
    $this->resourceTypes = $resourceTypes;
  }
  /**
   * @return ResourceTypes
   */
  public function getResourceTypes()
  {
    return $this->resourceTypes;
  }
  /**
   * @param GoogleCloudSecuritypostureV1PolicyRuleStringValues
   */
  public function setValues(GoogleCloudSecuritypostureV1PolicyRuleStringValues $values)
  {
    $this->values = $values;
  }
  /**
   * @return GoogleCloudSecuritypostureV1PolicyRuleStringValues
   */
  public function getValues()
  {
    return $this->values;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritypostureV1PolicyRule::class, 'Google_Service_SecurityPosture_GoogleCloudSecuritypostureV1PolicyRule');
