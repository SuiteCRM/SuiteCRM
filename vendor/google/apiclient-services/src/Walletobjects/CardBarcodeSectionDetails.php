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

namespace Google\Service\Walletobjects;

class CardBarcodeSectionDetails extends \Google\Model
{
  protected $firstBottomDetailType = BarcodeSectionDetail::class;
  protected $firstBottomDetailDataType = '';
  protected $firstTopDetailType = BarcodeSectionDetail::class;
  protected $firstTopDetailDataType = '';
  protected $secondTopDetailType = BarcodeSectionDetail::class;
  protected $secondTopDetailDataType = '';

  /**
   * @param BarcodeSectionDetail
   */
  public function setFirstBottomDetail(BarcodeSectionDetail $firstBottomDetail)
  {
    $this->firstBottomDetail = $firstBottomDetail;
  }
  /**
   * @return BarcodeSectionDetail
   */
  public function getFirstBottomDetail()
  {
    return $this->firstBottomDetail;
  }
  /**
   * @param BarcodeSectionDetail
   */
  public function setFirstTopDetail(BarcodeSectionDetail $firstTopDetail)
  {
    $this->firstTopDetail = $firstTopDetail;
  }
  /**
   * @return BarcodeSectionDetail
   */
  public function getFirstTopDetail()
  {
    return $this->firstTopDetail;
  }
  /**
   * @param BarcodeSectionDetail
   */
  public function setSecondTopDetail(BarcodeSectionDetail $secondTopDetail)
  {
    $this->secondTopDetail = $secondTopDetail;
  }
  /**
   * @return BarcodeSectionDetail
   */
  public function getSecondTopDetail()
  {
    return $this->secondTopDetail;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CardBarcodeSectionDetails::class, 'Google_Service_Walletobjects_CardBarcodeSectionDetails');
