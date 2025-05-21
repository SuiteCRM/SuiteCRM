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

namespace Google\Service\CloudAsset;

class AnalyzerOrgPolicy extends \Google\Collection
{
  protected $collection_key = 'rules';
  /**
   * @var string
   */
  public $appliedResource;
  /**
   * @var string
   */
  public $attachedResource;
  /**
   * @var bool
   */
  public $inheritFromParent;
  /**
   * @var bool
   */
  public $reset;
  protected $rulesType = GoogleCloudAssetV1Rule::class;
  protected $rulesDataType = 'array';

  /**
   * @param string
   */
  public function setAppliedResource($appliedResource)
  {
    $this->appliedResource = $appliedResource;
  }
  /**
   * @return string
   */
  public function getAppliedResource()
  {
    return $this->appliedResource;
  }
  /**
   * @param string
   */
  public function setAttachedResource($attachedResource)
  {
    $this->attachedResource = $attachedResource;
  }
  /**
   * @return string
   */
  public function getAttachedResource()
  {
    return $this->attachedResource;
  }
  /**
   * @param bool
   */
  public function setInheritFromParent($inheritFromParent)
  {
    $this->inheritFromParent = $inheritFromParent;
  }
  /**
   * @return bool
   */
  public function getInheritFromParent()
  {
    return $this->inheritFromParent;
  }
  /**
   * @param bool
   */
  public function setReset($reset)
  {
    $this->reset = $reset;
  }
  /**
   * @return bool
   */
  public function getReset()
  {
    return $this->reset;
  }
  /**
   * @param GoogleCloudAssetV1Rule[]
   */
  public function setRules($rules)
  {
    $this->rules = $rules;
  }
  /**
   * @return GoogleCloudAssetV1Rule[]
   */
  public function getRules()
  {
    return $this->rules;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AnalyzerOrgPolicy::class, 'Google_Service_CloudAsset_AnalyzerOrgPolicy');
