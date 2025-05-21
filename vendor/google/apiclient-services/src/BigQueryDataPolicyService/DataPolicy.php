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

namespace Google\Service\BigQueryDataPolicyService;

class DataPolicy extends \Google\Model
{
  protected $dataMaskingPolicyType = DataMaskingPolicy::class;
  protected $dataMaskingPolicyDataType = '';
  /**
   * @var string
   */
  public $dataPolicyId;
  /**
   * @var string
   */
  public $dataPolicyType;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $policyTag;

  /**
   * @param DataMaskingPolicy
   */
  public function setDataMaskingPolicy(DataMaskingPolicy $dataMaskingPolicy)
  {
    $this->dataMaskingPolicy = $dataMaskingPolicy;
  }
  /**
   * @return DataMaskingPolicy
   */
  public function getDataMaskingPolicy()
  {
    return $this->dataMaskingPolicy;
  }
  /**
   * @param string
   */
  public function setDataPolicyId($dataPolicyId)
  {
    $this->dataPolicyId = $dataPolicyId;
  }
  /**
   * @return string
   */
  public function getDataPolicyId()
  {
    return $this->dataPolicyId;
  }
  /**
   * @param string
   */
  public function setDataPolicyType($dataPolicyType)
  {
    $this->dataPolicyType = $dataPolicyType;
  }
  /**
   * @return string
   */
  public function getDataPolicyType()
  {
    return $this->dataPolicyType;
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
  public function setPolicyTag($policyTag)
  {
    $this->policyTag = $policyTag;
  }
  /**
   * @return string
   */
  public function getPolicyTag()
  {
    return $this->policyTag;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DataPolicy::class, 'Google_Service_BigQueryDataPolicyService_DataPolicy');
