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

namespace Google\Service\VMMigrationService;

class Source extends \Google\Model
{
  protected $awsType = AwsSourceDetails::class;
  protected $awsDataType = '';
  protected $azureType = AzureSourceDetails::class;
  protected $azureDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  protected $encryptionType = Encryption::class;
  protected $encryptionDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $updateTime;
  protected $vmwareType = VmwareSourceDetails::class;
  protected $vmwareDataType = '';

  /**
   * @param AwsSourceDetails
   */
  public function setAws(AwsSourceDetails $aws)
  {
    $this->aws = $aws;
  }
  /**
   * @return AwsSourceDetails
   */
  public function getAws()
  {
    return $this->aws;
  }
  /**
   * @param AzureSourceDetails
   */
  public function setAzure(AzureSourceDetails $azure)
  {
    $this->azure = $azure;
  }
  /**
   * @return AzureSourceDetails
   */
  public function getAzure()
  {
    return $this->azure;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param Encryption
   */
  public function setEncryption(Encryption $encryption)
  {
    $this->encryption = $encryption;
  }
  /**
   * @return Encryption
   */
  public function getEncryption()
  {
    return $this->encryption;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param VmwareSourceDetails
   */
  public function setVmware(VmwareSourceDetails $vmware)
  {
    $this->vmware = $vmware;
  }
  /**
   * @return VmwareSourceDetails
   */
  public function getVmware()
  {
    return $this->vmware;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Source::class, 'Google_Service_VMMigrationService_Source');
