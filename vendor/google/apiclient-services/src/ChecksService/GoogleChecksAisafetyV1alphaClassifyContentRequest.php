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

namespace Google\Service\ChecksService;

class GoogleChecksAisafetyV1alphaClassifyContentRequest extends \Google\Collection
{
  protected $collection_key = 'policies';
  /**
   * @var string
   */
  public $classifierVersion;
  protected $contextType = GoogleChecksAisafetyV1alphaClassifyContentRequestContext::class;
  protected $contextDataType = '';
  protected $inputType = GoogleChecksAisafetyV1alphaClassifyContentRequestInputContent::class;
  protected $inputDataType = '';
  protected $policiesType = GoogleChecksAisafetyV1alphaClassifyContentRequestPolicyConfig::class;
  protected $policiesDataType = 'array';

  /**
   * @param string
   */
  public function setClassifierVersion($classifierVersion)
  {
    $this->classifierVersion = $classifierVersion;
  }
  /**
   * @return string
   */
  public function getClassifierVersion()
  {
    return $this->classifierVersion;
  }
  /**
   * @param GoogleChecksAisafetyV1alphaClassifyContentRequestContext
   */
  public function setContext(GoogleChecksAisafetyV1alphaClassifyContentRequestContext $context)
  {
    $this->context = $context;
  }
  /**
   * @return GoogleChecksAisafetyV1alphaClassifyContentRequestContext
   */
  public function getContext()
  {
    return $this->context;
  }
  /**
   * @param GoogleChecksAisafetyV1alphaClassifyContentRequestInputContent
   */
  public function setInput(GoogleChecksAisafetyV1alphaClassifyContentRequestInputContent $input)
  {
    $this->input = $input;
  }
  /**
   * @return GoogleChecksAisafetyV1alphaClassifyContentRequestInputContent
   */
  public function getInput()
  {
    return $this->input;
  }
  /**
   * @param GoogleChecksAisafetyV1alphaClassifyContentRequestPolicyConfig[]
   */
  public function setPolicies($policies)
  {
    $this->policies = $policies;
  }
  /**
   * @return GoogleChecksAisafetyV1alphaClassifyContentRequestPolicyConfig[]
   */
  public function getPolicies()
  {
    return $this->policies;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChecksAisafetyV1alphaClassifyContentRequest::class, 'Google_Service_ChecksService_GoogleChecksAisafetyV1alphaClassifyContentRequest');
