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

namespace Google\Service\Transcoder;

class Encryption extends \Google\Model
{
  protected $aes128Type = Aes128Encryption::class;
  protected $aes128DataType = '';
  protected $drmSystemsType = DrmSystems::class;
  protected $drmSystemsDataType = '';
  /**
   * @var string
   */
  public $id;
  protected $mpegCencType = MpegCommonEncryption::class;
  protected $mpegCencDataType = '';
  protected $sampleAesType = SampleAesEncryption::class;
  protected $sampleAesDataType = '';
  protected $secretManagerKeySourceType = SecretManagerSource::class;
  protected $secretManagerKeySourceDataType = '';

  /**
   * @param Aes128Encryption
   */
  public function setAes128(Aes128Encryption $aes128)
  {
    $this->aes128 = $aes128;
  }
  /**
   * @return Aes128Encryption
   */
  public function getAes128()
  {
    return $this->aes128;
  }
  /**
   * @param DrmSystems
   */
  public function setDrmSystems(DrmSystems $drmSystems)
  {
    $this->drmSystems = $drmSystems;
  }
  /**
   * @return DrmSystems
   */
  public function getDrmSystems()
  {
    return $this->drmSystems;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param MpegCommonEncryption
   */
  public function setMpegCenc(MpegCommonEncryption $mpegCenc)
  {
    $this->mpegCenc = $mpegCenc;
  }
  /**
   * @return MpegCommonEncryption
   */
  public function getMpegCenc()
  {
    return $this->mpegCenc;
  }
  /**
   * @param SampleAesEncryption
   */
  public function setSampleAes(SampleAesEncryption $sampleAes)
  {
    $this->sampleAes = $sampleAes;
  }
  /**
   * @return SampleAesEncryption
   */
  public function getSampleAes()
  {
    return $this->sampleAes;
  }
  /**
   * @param SecretManagerSource
   */
  public function setSecretManagerKeySource(SecretManagerSource $secretManagerKeySource)
  {
    $this->secretManagerKeySource = $secretManagerKeySource;
  }
  /**
   * @return SecretManagerSource
   */
  public function getSecretManagerKeySource()
  {
    return $this->secretManagerKeySource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Encryption::class, 'Google_Service_Transcoder_Encryption');
