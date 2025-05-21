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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1AspectTypeMetadataTemplate extends \Google\Collection
{
  protected $collection_key = 'recordFields';
  protected $annotationsType = GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations::class;
  protected $annotationsDataType = '';
  protected $arrayItemsType = GoogleCloudDataplexV1AspectTypeMetadataTemplate::class;
  protected $arrayItemsDataType = '';
  protected $constraintsType = GoogleCloudDataplexV1AspectTypeMetadataTemplateConstraints::class;
  protected $constraintsDataType = '';
  protected $enumValuesType = GoogleCloudDataplexV1AspectTypeMetadataTemplateEnumValue::class;
  protected $enumValuesDataType = 'array';
  /**
   * @var int
   */
  public $index;
  protected $mapItemsType = GoogleCloudDataplexV1AspectTypeMetadataTemplate::class;
  protected $mapItemsDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $recordFieldsType = GoogleCloudDataplexV1AspectTypeMetadataTemplate::class;
  protected $recordFieldsDataType = 'array';
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $typeId;
  /**
   * @var string
   */
  public $typeRef;

  /**
   * @param GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations
   */
  public function setAnnotations(GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations $annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param GoogleCloudDataplexV1AspectTypeMetadataTemplate
   */
  public function setArrayItems(GoogleCloudDataplexV1AspectTypeMetadataTemplate $arrayItems)
  {
    $this->arrayItems = $arrayItems;
  }
  /**
   * @return GoogleCloudDataplexV1AspectTypeMetadataTemplate
   */
  public function getArrayItems()
  {
    return $this->arrayItems;
  }
  /**
   * @param GoogleCloudDataplexV1AspectTypeMetadataTemplateConstraints
   */
  public function setConstraints(GoogleCloudDataplexV1AspectTypeMetadataTemplateConstraints $constraints)
  {
    $this->constraints = $constraints;
  }
  /**
   * @return GoogleCloudDataplexV1AspectTypeMetadataTemplateConstraints
   */
  public function getConstraints()
  {
    return $this->constraints;
  }
  /**
   * @param GoogleCloudDataplexV1AspectTypeMetadataTemplateEnumValue[]
   */
  public function setEnumValues($enumValues)
  {
    $this->enumValues = $enumValues;
  }
  /**
   * @return GoogleCloudDataplexV1AspectTypeMetadataTemplateEnumValue[]
   */
  public function getEnumValues()
  {
    return $this->enumValues;
  }
  /**
   * @param int
   */
  public function setIndex($index)
  {
    $this->index = $index;
  }
  /**
   * @return int
   */
  public function getIndex()
  {
    return $this->index;
  }
  /**
   * @param GoogleCloudDataplexV1AspectTypeMetadataTemplate
   */
  public function setMapItems(GoogleCloudDataplexV1AspectTypeMetadataTemplate $mapItems)
  {
    $this->mapItems = $mapItems;
  }
  /**
   * @return GoogleCloudDataplexV1AspectTypeMetadataTemplate
   */
  public function getMapItems()
  {
    return $this->mapItems;
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
   * @param GoogleCloudDataplexV1AspectTypeMetadataTemplate[]
   */
  public function setRecordFields($recordFields)
  {
    $this->recordFields = $recordFields;
  }
  /**
   * @return GoogleCloudDataplexV1AspectTypeMetadataTemplate[]
   */
  public function getRecordFields()
  {
    return $this->recordFields;
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
  public function setTypeId($typeId)
  {
    $this->typeId = $typeId;
  }
  /**
   * @return string
   */
  public function getTypeId()
  {
    return $this->typeId;
  }
  /**
   * @param string
   */
  public function setTypeRef($typeRef)
  {
    $this->typeRef = $typeRef;
  }
  /**
   * @return string
   */
  public function getTypeRef()
  {
    return $this->typeRef;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1AspectTypeMetadataTemplate::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1AspectTypeMetadataTemplate');
