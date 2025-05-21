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

namespace Google\Service\OrgPolicyAPI;

class GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameter extends \Google\Model
{
  /**
   * @var array
   */
  public $defaultValue;
  /**
   * @var string
   */
  public $item;
  protected $metadataType = GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameterMetadata::class;
  protected $metadataDataType = '';
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $validValuesExpr;

  /**
   * @param array
   */
  public function setDefaultValue($defaultValue)
  {
    $this->defaultValue = $defaultValue;
  }
  /**
   * @return array
   */
  public function getDefaultValue()
  {
    return $this->defaultValue;
  }
  /**
   * @param string
   */
  public function setItem($item)
  {
    $this->item = $item;
  }
  /**
   * @return string
   */
  public function getItem()
  {
    return $this->item;
  }
  /**
   * @param GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameterMetadata
   */
  public function setMetadata(GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameterMetadata $metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameterMetadata
   */
  public function getMetadata()
  {
    return $this->metadata;
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
  /**
   * @param string
   */
  public function setValidValuesExpr($validValuesExpr)
  {
    $this->validValuesExpr = $validValuesExpr;
  }
  /**
   * @return string
   */
  public function getValidValuesExpr()
  {
    return $this->validValuesExpr;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameter::class, 'Google_Service_OrgPolicyAPI_GoogleCloudOrgpolicyV2ConstraintCustomConstraintDefinitionParameter');
