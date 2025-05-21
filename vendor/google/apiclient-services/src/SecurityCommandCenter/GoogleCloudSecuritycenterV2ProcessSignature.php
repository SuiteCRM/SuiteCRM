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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2ProcessSignature extends \Google\Model
{
  protected $memoryHashSignatureType = GoogleCloudSecuritycenterV2MemoryHashSignature::class;
  protected $memoryHashSignatureDataType = '';
  /**
   * @var string
   */
  public $signatureType;
  protected $yaraRuleSignatureType = GoogleCloudSecuritycenterV2YaraRuleSignature::class;
  protected $yaraRuleSignatureDataType = '';

  /**
   * @param GoogleCloudSecuritycenterV2MemoryHashSignature
   */
  public function setMemoryHashSignature(GoogleCloudSecuritycenterV2MemoryHashSignature $memoryHashSignature)
  {
    $this->memoryHashSignature = $memoryHashSignature;
  }
  /**
   * @return GoogleCloudSecuritycenterV2MemoryHashSignature
   */
  public function getMemoryHashSignature()
  {
    return $this->memoryHashSignature;
  }
  /**
   * @param string
   */
  public function setSignatureType($signatureType)
  {
    $this->signatureType = $signatureType;
  }
  /**
   * @return string
   */
  public function getSignatureType()
  {
    return $this->signatureType;
  }
  /**
   * @param GoogleCloudSecuritycenterV2YaraRuleSignature
   */
  public function setYaraRuleSignature(GoogleCloudSecuritycenterV2YaraRuleSignature $yaraRuleSignature)
  {
    $this->yaraRuleSignature = $yaraRuleSignature;
  }
  /**
   * @return GoogleCloudSecuritycenterV2YaraRuleSignature
   */
  public function getYaraRuleSignature()
  {
    return $this->yaraRuleSignature;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2ProcessSignature::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2ProcessSignature');
