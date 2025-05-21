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

namespace Google\Service\CloudNaturalLanguage;

class XPSDataType extends \Google\Collection
{
  protected $collection_key = 'compatibleDataTypes';
  protected $compatibleDataTypesType = XPSDataType::class;
  protected $compatibleDataTypesDataType = 'array';
  protected $listElementTypeType = XPSDataType::class;
  protected $listElementTypeDataType = '';
  /**
   * @var bool
   */
  public $nullable;
  protected $structTypeType = XPSStructType::class;
  protected $structTypeDataType = '';
  /**
   * @var string
   */
  public $timeFormat;
  /**
   * @var string
   */
  public $typeCode;

  /**
   * @param XPSDataType[]
   */
  public function setCompatibleDataTypes($compatibleDataTypes)
  {
    $this->compatibleDataTypes = $compatibleDataTypes;
  }
  /**
   * @return XPSDataType[]
   */
  public function getCompatibleDataTypes()
  {
    return $this->compatibleDataTypes;
  }
  /**
   * @param XPSDataType
   */
  public function setListElementType(XPSDataType $listElementType)
  {
    $this->listElementType = $listElementType;
  }
  /**
   * @return XPSDataType
   */
  public function getListElementType()
  {
    return $this->listElementType;
  }
  /**
   * @param bool
   */
  public function setNullable($nullable)
  {
    $this->nullable = $nullable;
  }
  /**
   * @return bool
   */
  public function getNullable()
  {
    return $this->nullable;
  }
  /**
   * @param XPSStructType
   */
  public function setStructType(XPSStructType $structType)
  {
    $this->structType = $structType;
  }
  /**
   * @return XPSStructType
   */
  public function getStructType()
  {
    return $this->structType;
  }
  /**
   * @param string
   */
  public function setTimeFormat($timeFormat)
  {
    $this->timeFormat = $timeFormat;
  }
  /**
   * @return string
   */
  public function getTimeFormat()
  {
    return $this->timeFormat;
  }
  /**
   * @param string
   */
  public function setTypeCode($typeCode)
  {
    $this->typeCode = $typeCode;
  }
  /**
   * @return string
   */
  public function getTypeCode()
  {
    return $this->typeCode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSDataType::class, 'Google_Service_CloudNaturalLanguage_XPSDataType');
