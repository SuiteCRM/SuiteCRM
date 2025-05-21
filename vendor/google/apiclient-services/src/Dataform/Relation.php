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

class Relation extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string[]
   */
  public $additionalOptions;
  /**
   * @var string[]
   */
  public $clusterExpressions;
  protected $dependencyTargetsType = Target::class;
  protected $dependencyTargetsDataType = 'array';
  /**
   * @var bool
   */
  public $disabled;
  protected $incrementalTableConfigType = IncrementalTableConfig::class;
  protected $incrementalTableConfigDataType = '';
  /**
   * @var int
   */
  public $partitionExpirationDays;
  /**
   * @var string
   */
  public $partitionExpression;
  /**
   * @var string[]
   */
  public $postOperations;
  /**
   * @var string[]
   */
  public $preOperations;
  protected $relationDescriptorType = RelationDescriptor::class;
  protected $relationDescriptorDataType = '';
  /**
   * @var string
   */
  public $relationType;
  /**
   * @var bool
   */
  public $requirePartitionFilter;
  /**
   * @var string
   */
  public $selectQuery;
  /**
   * @var string[]
   */
  public $tags;

  /**
   * @param string[]
   */
  public function setAdditionalOptions($additionalOptions)
  {
    $this->additionalOptions = $additionalOptions;
  }
  /**
   * @return string[]
   */
  public function getAdditionalOptions()
  {
    return $this->additionalOptions;
  }
  /**
   * @param string[]
   */
  public function setClusterExpressions($clusterExpressions)
  {
    $this->clusterExpressions = $clusterExpressions;
  }
  /**
   * @return string[]
   */
  public function getClusterExpressions()
  {
    return $this->clusterExpressions;
  }
  /**
   * @param Target[]
   */
  public function setDependencyTargets($dependencyTargets)
  {
    $this->dependencyTargets = $dependencyTargets;
  }
  /**
   * @return Target[]
   */
  public function getDependencyTargets()
  {
    return $this->dependencyTargets;
  }
  /**
   * @param bool
   */
  public function setDisabled($disabled)
  {
    $this->disabled = $disabled;
  }
  /**
   * @return bool
   */
  public function getDisabled()
  {
    return $this->disabled;
  }
  /**
   * @param IncrementalTableConfig
   */
  public function setIncrementalTableConfig(IncrementalTableConfig $incrementalTableConfig)
  {
    $this->incrementalTableConfig = $incrementalTableConfig;
  }
  /**
   * @return IncrementalTableConfig
   */
  public function getIncrementalTableConfig()
  {
    return $this->incrementalTableConfig;
  }
  /**
   * @param int
   */
  public function setPartitionExpirationDays($partitionExpirationDays)
  {
    $this->partitionExpirationDays = $partitionExpirationDays;
  }
  /**
   * @return int
   */
  public function getPartitionExpirationDays()
  {
    return $this->partitionExpirationDays;
  }
  /**
   * @param string
   */
  public function setPartitionExpression($partitionExpression)
  {
    $this->partitionExpression = $partitionExpression;
  }
  /**
   * @return string
   */
  public function getPartitionExpression()
  {
    return $this->partitionExpression;
  }
  /**
   * @param string[]
   */
  public function setPostOperations($postOperations)
  {
    $this->postOperations = $postOperations;
  }
  /**
   * @return string[]
   */
  public function getPostOperations()
  {
    return $this->postOperations;
  }
  /**
   * @param string[]
   */
  public function setPreOperations($preOperations)
  {
    $this->preOperations = $preOperations;
  }
  /**
   * @return string[]
   */
  public function getPreOperations()
  {
    return $this->preOperations;
  }
  /**
   * @param RelationDescriptor
   */
  public function setRelationDescriptor(RelationDescriptor $relationDescriptor)
  {
    $this->relationDescriptor = $relationDescriptor;
  }
  /**
   * @return RelationDescriptor
   */
  public function getRelationDescriptor()
  {
    return $this->relationDescriptor;
  }
  /**
   * @param string
   */
  public function setRelationType($relationType)
  {
    $this->relationType = $relationType;
  }
  /**
   * @return string
   */
  public function getRelationType()
  {
    return $this->relationType;
  }
  /**
   * @param bool
   */
  public function setRequirePartitionFilter($requirePartitionFilter)
  {
    $this->requirePartitionFilter = $requirePartitionFilter;
  }
  /**
   * @return bool
   */
  public function getRequirePartitionFilter()
  {
    return $this->requirePartitionFilter;
  }
  /**
   * @param string
   */
  public function setSelectQuery($selectQuery)
  {
    $this->selectQuery = $selectQuery;
  }
  /**
   * @return string
   */
  public function getSelectQuery()
  {
    return $this->selectQuery;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Relation::class, 'Google_Service_Dataform_Relation');
