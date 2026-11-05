<?php
$module_name = 'AOS_Quotes';
$_object_name = 'aos_quotes';
$viewdefs [$module_name] =
array(
  'EditView' =>
  array(
    'templateMeta' =>
    array(
      'form' =>
      array(
        'buttons' =>
        array(
          0 => 'SAVE',
          1 => 'CANCEL',
        ),
      ),
      'maxColumns' => '2',
      'widths' =>
      array(
        0 =>
        array(
          'label' => '10',
          'field' => '30',
        ),
        1 =>
        array(
          'label' => '10',
          'field' => '30',
        ),
      ),
      'useTabs' => false,
      'tabDefs' =>
      array(
        'LBL_ACCOUNT_INFORMATION' =>
        array(
          'newTab' => false,
          'panelDefault' => 'expanded',
        ),
        'LBL_ADDRESS_INFORMATION' =>
        array(
          'newTab' => false,
          'panelDefault' => 'expanded',
        ),
        'LBL_LINE_ITEMS' =>
        array(
          'newTab' => false,
          'panelDefault' => 'expanded',
        ),
      ),
    ),
    'panels' =>
    array(
      'lbl_account_information' =>
      array(
        0 =>
        array(
          0 =>
          array(
            'name' => 'name',
            'displayParams' =>
            array(
              'required' => true,
            ),
            'label' => 'LBL_NAME',
          ),
          1 =>
          array(
            'name' => 'opportunity',
            'label' => 'LBL_OPPORTUNITY',
          ),
        ),
        1 =>
        array(
          0 =>
          array(
            'name' => 'number',
            'label' => 'LBL_QUOTE_NUMBER',
            'customCode' => '{$fields.number.value}',
          ),
          1 =>
          array(
            'name' => 'stage',
            'label' => 'LBL_STAGE',
          ),
        ),
        2 =>
        array(
          0 =>
          array(
            'name' => 'expiration',
            'label' => 'LBL_EXPIRATION',
          ),
          1 =>
          array(
            'name' => 'invoice_status',
            'label' => 'LBL_INVOICE_STATUS',
          ),
        ),
        3 =>
        array(
          0 =>
          array(
            'name' => 'assigned_user_name',
            'label' => 'LBL_ASSIGNED_TO_NAME',
          ),
          1 =>
          array(
            'name' => 'term',
            'label' => 'LBL_TERM',
          ),
        ),
        4 =>
        array(
          0 =>
          array(
            'name' => 'approval_status',
            'label' => 'LBL_APPROVAL_STATUS',
          ),
          1 =>
          array(
            'name' => 'approval_issue',
            'label' => 'LBL_APPROVAL_ISSUE',
          ),
        ),
      ),
      'lbl_address_information' =>
      array(
        0 =>
        array(
          0 =>
          array(
            'name' => 'billing_account',
            'label' => 'LBL_BILLING_ACCOUNT',
            'displayParams' =>
            array(
              'key' =>
              array(
                0 => 'billing',
                1 => 'shipping',
              ),
              'copy' =>
              array(
                0 => 'billing',
                1 => 'shipping',
              ),
              'billingKey' => 'billing',
              'shippingKey' => 'shipping',
            ),
          ),
          1 => '',
        ),
        1 =>
        array(
          0 =>
          array(
            'name' => 'billing_contact',
            'label' => 'LBL_BILLING_CONTACT',
            'displayParams' =>
            array(
              'initial_filter' => '&account_name="+encodeURIComponent(this.form.{$fields.billing_account.name}.value)+"',
            ),
          ),
          1 => '',
        ),
        2 =>
        array(
          0 =>
          array(
            'name' => 'billing_address_street',
            'hideLabel' => true,
            'type' => 'address',
            'displayParams' =>
            array(
              'key' => 'billing',
              'rows' => 2,
              'cols' => 30,
              'maxlength' => 150,
            ),
            'label' => 'LBL_BILLING_ADDRESS_STREET',
          ),
          1 =>
          array(
            'name' => 'shipping_address_street',
            'hideLabel' => true,
            'type' => 'address',
            'displayParams' =>
            array(
              'key' => 'shipping',
              'copy' => 'billing',
              'rows' => 2,
              'cols' => 30,
              'maxlength' => 150,
            ),
            'label' => 'LBL_SHIPPING_ADDRESS_STREET',
          ),
        ),
      ),
      'lbl_line_items' =>
      array(
        0 =>
        array(
          0 =>
          array(
            'name' => 'currency_id',
            'studio' => 'visible',
            'label' => 'LBL_CURRENCY',
          ),
        ),
        1 =>
        array(
          0 =>
          array(
            'name' => 'line_items',
            'label' => 'LBL_LINE_ITEMS',
          ),
        ),
        2 =>
        array(
          0 => '',
        ),
        3 =>
        array(
          0 =>
          array(
            'name' => 'total_amt',
            'label' => 'LBL_TOTAL_AMT',
          ),
        ),
        4 =>
        array(
          0 =>
          array(
            'name' => 'discount_amount',
            'label' => 'LBL_DISCOUNT_AMOUNT',
          ),
        ),
        5 =>
        array(
          0 =>
          array(
            'name' => 'subtotal_amount',
            'label' => 'LBL_SUBTOTAL_AMOUNT',
          ),
        ),
        6 =>
        array(
          0 =>
          array(
            'name' => 'shipping_amount',
            'label' => 'LBL_SHIPPING_AMOUNT',
            'displayParams' =>
            array(
              'field' =>
              array(
                'onblur' => 'calculateTotal(\'lineItems\');',
              ),
            ),
          ),
        ),
        7 =>
        array(
          0 =>
          array(
            'name' => 'shipping_tax_amt',
            'label' => 'LBL_SHIPPING_TAX_AMT',
          ),
        ),
        8 =>
        array(
          0 =>
          array(
            'name' => 'tax_amount',
            'label' => 'LBL_TAX_AMOUNT',
          ),
        ),
        9 =>
        array(
          0 =>
          array(
            'name' => 'total_amount',
            'label' => 'LBL_GRAND_TOTAL',
          ),
        ),
      ),
    ),
  ),
);
