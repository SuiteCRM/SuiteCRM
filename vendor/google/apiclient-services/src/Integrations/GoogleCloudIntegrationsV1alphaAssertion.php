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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaAssertion extends \Google\Model
{
  /**
   * @var string
   */
  public $assertionStrategy;
  /**
   * @var string
   */
  public $condition;
  protected $parameterType = GoogleCloudIntegrationsV1alphaEventParameter::class;
  protected $parameterDataType = '';
  /**
   * @var int
   */
  public $retryCount;

  /**
   * @param string
   */
  public function setAssertionStrategy($assertionStrategy)
  {
    $this->assertionStrategy = $assertionStrategy;
  }
  /**
   * @return string
   */
  public function getAssertionStrategy()
  {
    return $this->assertionStrategy;
  }
  /**
   * @param string
   */
  public function setCondition($condition)
  {
    $this->condition = $condition;
  }
  /**
   * @return string
   */
  public function getCondition()
  {
    return $this->condition;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaEventParameter
   */
  public function setParameter(GoogleCloudIntegrationsV1alphaEventParameter $parameter)
  {
    $this->parameter = $parameter;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaEventParameter
   */
  public function getParameter()
  {
    return $this->parameter;
  }
  /**
   * @param int
   */
  public function setRetryCount($retryCount)
  {
    $this->retryCount = $retryCount;
  }
  /**
   * @return int
   */
  public function getRetryCount()
  {
    return $this->retryCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaAssertion::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaAssertion');
