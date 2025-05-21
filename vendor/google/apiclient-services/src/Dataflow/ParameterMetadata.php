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

namespace Google\Service\Dataflow;

class ParameterMetadata extends \Google\Collection
{
  protected $collection_key = 'regexes';
  /**
   * @var string[]
   */
  public $customMetadata;
  /**
   * @var string
   */
  public $defaultValue;
  protected $enumOptionsType = ParameterMetadataEnumOption::class;
  protected $enumOptionsDataType = 'array';
  /**
   * @var string
   */
  public $groupName;
  /**
   * @var string
   */
  public $helpText;
  /**
   * @var bool
   */
  public $hiddenUi;
  /**
   * @var bool
   */
  public $isOptional;
  /**
   * @var string
   */
  public $label;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $paramType;
  /**
   * @var string
   */
  public $parentName;
  /**
   * @var string[]
   */
  public $parentTriggerValues;
  /**
   * @var string[]
   */
  public $regexes;

  /**
   * @param string[]
   */
  public function setCustomMetadata($customMetadata)
  {
    $this->customMetadata = $customMetadata;
  }
  /**
   * @return string[]
   */
  public function getCustomMetadata()
  {
    return $this->customMetadata;
  }
  /**
   * @param string
   */
  public function setDefaultValue($defaultValue)
  {
    $this->defaultValue = $defaultValue;
  }
  /**
   * @return string
   */
  public function getDefaultValue()
  {
    return $this->defaultValue;
  }
  /**
   * @param ParameterMetadataEnumOption[]
   */
  public function setEnumOptions($enumOptions)
  {
    $this->enumOptions = $enumOptions;
  }
  /**
   * @return ParameterMetadataEnumOption[]
   */
  public function getEnumOptions()
  {
    return $this->enumOptions;
  }
  /**
   * @param string
   */
  public function setGroupName($groupName)
  {
    $this->groupName = $groupName;
  }
  /**
   * @return string
   */
  public function getGroupName()
  {
    return $this->groupName;
  }
  /**
   * @param string
   */
  public function setHelpText($helpText)
  {
    $this->helpText = $helpText;
  }
  /**
   * @return string
   */
  public function getHelpText()
  {
    return $this->helpText;
  }
  /**
   * @param bool
   */
  public function setHiddenUi($hiddenUi)
  {
    $this->hiddenUi = $hiddenUi;
  }
  /**
   * @return bool
   */
  public function getHiddenUi()
  {
    return $this->hiddenUi;
  }
  /**
   * @param bool
   */
  public function setIsOptional($isOptional)
  {
    $this->isOptional = $isOptional;
  }
  /**
   * @return bool
   */
  public function getIsOptional()
  {
    return $this->isOptional;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
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
   * @param string
   */
  public function setParamType($paramType)
  {
    $this->paramType = $paramType;
  }
  /**
   * @return string
   */
  public function getParamType()
  {
    return $this->paramType;
  }
  /**
   * @param string
   */
  public function setParentName($parentName)
  {
    $this->parentName = $parentName;
  }
  /**
   * @return string
   */
  public function getParentName()
  {
    return $this->parentName;
  }
  /**
   * @param string[]
   */
  public function setParentTriggerValues($parentTriggerValues)
  {
    $this->parentTriggerValues = $parentTriggerValues;
  }
  /**
   * @return string[]
   */
  public function getParentTriggerValues()
  {
    return $this->parentTriggerValues;
  }
  /**
   * @param string[]
   */
  public function setRegexes($regexes)
  {
    $this->regexes = $regexes;
  }
  /**
   * @return string[]
   */
  public function getRegexes()
  {
    return $this->regexes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ParameterMetadata::class, 'Google_Service_Dataflow_ParameterMetadata');
