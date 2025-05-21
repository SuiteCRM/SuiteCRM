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

class Operations extends \Google\Collection
{
  protected $collection_key = 'tags';
  protected $dependencyTargetsType = Target::class;
  protected $dependencyTargetsDataType = 'array';
  /**
   * @var bool
   */
  public $disabled;
  /**
   * @var bool
   */
  public $hasOutput;
  /**
   * @var string[]
   */
  public $queries;
  protected $relationDescriptorType = RelationDescriptor::class;
  protected $relationDescriptorDataType = '';
  /**
   * @var string[]
   */
  public $tags;

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
   * @param bool
   */
  public function setHasOutput($hasOutput)
  {
    $this->hasOutput = $hasOutput;
  }
  /**
   * @return bool
   */
  public function getHasOutput()
  {
    return $this->hasOutput;
  }
  /**
   * @param string[]
   */
  public function setQueries($queries)
  {
    $this->queries = $queries;
  }
  /**
   * @return string[]
   */
  public function getQueries()
  {
    return $this->queries;
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
class_alias(Operations::class, 'Google_Service_Dataform_Operations');
